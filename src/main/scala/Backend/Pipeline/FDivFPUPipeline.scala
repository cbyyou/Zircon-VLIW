import chisel3._
import chisel3.util._

// FDivFPUPipeline 特有的 Hazard IO（包含 FDiv busy 信号）
class FDivFPUPipelineHazardIO extends PipelineHazardIO {
    val fdivBusy = Output(Bool())
}

class FDivFPUPipelineIO extends Bundle {
    val forward = new PipelineForwardIO
    val backend = new PipelineBackendIO
    val frontend = new PipelineFrontendIO
    val hazard = new FDivFPUPipelineHazardIO
}

class FDivFPUPipeline extends Module {
    val io = IO(new FDivFPUPipelineIO)
    
    // ========== EX1阶段 ==========
    // ID-EX1 段间寄存器在 Backend 中统一管理，这里直接使用传入的数据
    val ex1Pkg = io.backend.instPkgIn
    
    // 应用Forward前递
    val ex1Rs1Data = io.forward.fwdRs1Data
    val ex1Rs2Data = io.forward.fwdRs2Data
    val ex1Rs3Data = io.forward.fwdRs3Data
    
    // ALU实例化（用于支持基本ALU操作）
    val alu = Module(new ALU)
    val aluSrc1 = Mux(ex1Pkg.src1Sel === 0.U, ex1Rs1Data, ex1Pkg.pc)
    val aluSrc2 = Mux(ex1Pkg.src2Sel === 0.U, ex1Rs2Data, ex1Pkg.imm)
    alu.io.src1 := aluSrc1
    alu.io.src2 := aluSrc2
    alu.io.op := ex1Pkg.op
    
    // FDiv实例化
    val fdiv = Module(new FDivWrapper)
    fdiv.io.rs1Data := ex1Rs1Data
    fdiv.io.rs2Data := ex1Rs2Data
    fdiv.io.op := ex1Pkg.op
    fdiv.io.rm := ex1Pkg.rm
    // 判断是否是 FDiv 指令
    val isFDivOp = ex1Pkg.op === ZirconConfig.EXEOp.FDIV_S || 
                   ex1Pkg.op === ZirconConfig.EXEOp.FSQRT_S
    fdiv.io.valid := ex1Pkg.rdValid && isFDivOp
    // 分支预测失败或流水线冲刷时终止 FDiv 运算
    fdiv.io.kill := io.hazard.ex1Flush || io.hazard.ex2Flush || io.hazard.ex3Flush
    
    // FPU实例化
    val fpu = Module(new FPU)
    fpu.io.rs1Data := ex1Rs1Data
    fpu.io.rs2Data := ex1Rs2Data
    fpu.io.rs3Data := ex1Rs3Data
    fpu.io.op := ex1Pkg.op
    fpu.io.rm := ex1Pkg.rm
    
    // 组合浮点操作在EX1末保存；FADD和FMUL按内部延迟在后续阶段保存。
    val ex1PkgOut = ex1Pkg.EX1Update(alu.io.res, 0.U, false.B)
    val ex1IsAdd = ex1Pkg.op === ZirconConfig.EXEOp.FADD_S ||
                   ex1Pkg.op === ZirconConfig.EXEOp.FSUB_S
    val ex1IsMul = ex1Pkg.op === ZirconConfig.EXEOp.FMUL_S
    val ex1IsImmediateFpu = ex1Pkg.op(6) && !ex1IsAdd && !ex1IsMul
    val ex1PkgOutWithFpu = Mux(
        ex1IsImmediateFpu,
        ex1PkgOut.EX3Update(fpu.io.res, fpu.io.fflags),
        ex1PkgOut
    )
    
    // ========== EX2阶段 ==========
    val ex2Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex2Flush) {
        ex2Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex2Stall) {
        ex2Pkg := ex1PkgOutWithFpu
    }
    
    // ========== EX3阶段 ==========
    val ex3Pkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.ex3Flush) {
        ex3Pkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.ex3Stall) {
        // EX3阶段更新FDiv或内部一级流水的FADD结果。
        val isFDiv = ex2Pkg.op === ZirconConfig.EXEOp.FDIV_S || 
                     ex2Pkg.op === ZirconConfig.EXEOp.FSQRT_S
        val isFAdd = ex2Pkg.op === ZirconConfig.EXEOp.FADD_S ||
                     ex2Pkg.op === ZirconConfig.EXEOp.FSUB_S
        ex3Pkg := MuxCase(ex2Pkg, Seq(
            isFDiv -> ex2Pkg.EX3Update(fdiv.io.res, fdiv.io.fflags),
            isFAdd -> ex2Pkg.EX3Update(fpu.io.faddResult, fpu.io.faddFflags)
        ))
    }
    
    // ========== WB阶段 ==========
    val wbPkg = RegInit(0.U.asTypeOf(new InstructionPackage))
    when(io.hazard.wbFlush) {
        wbPkg := 0.U.asTypeOf(new InstructionPackage)
    }.elsewhen(!io.hazard.wbStall) {
        val ex3IsMul = ex3Pkg.op === ZirconConfig.EXEOp.FMUL_S
        wbPkg := Mux(
            ex3IsMul,
            ex3Pkg.EX3Update(fpu.io.fmulResult, fpu.io.fmulFflags),
            ex3Pkg
        )
    }
    
    // WB阶段：根据rd类型选择写回数据
    // 浮点比较/转换也可能写GPR，写回数据必须按操作类型选择。
    val isGPR = !wbPkg.rd(5)
    val isFpuResult = wbPkg.op(6) || wbPkg.op === ZirconConfig.EXEOp.FDIV_S ||
                      wbPkg.op === ZirconConfig.EXEOp.FSQRT_S
    val wbData = Mux(isFpuResult, wbPkg.fpuResult, wbPkg.aluResult)
    val wbPkgOut = wbPkg.WBUpdate(wbData)
    
    // 写回到寄存器堆
    io.frontend.gprWen := wbPkgOut.rdValid && isGPR
    io.frontend.gprWaddr := wbPkgOut.rd(4, 0)
    io.frontend.gprWdata := wbPkgOut.rfWdata
    io.frontend.fprWen := wbPkgOut.rdValid && !isGPR
    io.frontend.fprWaddr := wbPkgOut.rd(4, 0)
    io.frontend.fprWdata := wbPkgOut.rfWdata
    
    // 输出到Forward和Hazard
    // 注意：FDivFPUPipeline在EX2和EX3阶段不能前递（根据文档表格）
    io.forward.ex1Pkg := ex1Pkg
    io.forward.ex2Pkg := ex2Pkg
    io.forward.ex3Pkg := ex3Pkg
    io.forward.wbPkg := wbPkgOut
    io.hazard.ex1Pkg := ex1Pkg
    io.hazard.ex2Pkg := ex2Pkg
    
    // FDiv busy 信号
    io.hazard.fdivBusy := fdiv.io.busy
}
