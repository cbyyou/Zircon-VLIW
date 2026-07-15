import chisel3._
import chisel3.util._

// Forward模块的IO
class ForwardIO extends Bundle {
    // 8条流水线的EX2阶段InstPkg（用于判断前递）
    val ex2Pkgs = Input(Vec(8, new InstructionPackage))
    // 8条流水线的EX3阶段InstPkg（用于判断前递）
    val ex3Pkgs = Input(Vec(8, new InstructionPackage))
    // 8条流水线的WB阶段InstPkg（用于判断前递）
    val wbPkgs = Input(Vec(8, new InstructionPackage))
    
    // 8条流水线的EX1阶段InstPkg（需要前递的目标）
    val ex1Pkgs = Input(Vec(8, new InstructionPackage))
    
    // 前递后的数据输出（rs1Data, rs2Data, rs3Data）
    val fwdRs1Data = Output(Vec(8, UInt(32.W)))
    val fwdRs2Data = Output(Vec(8, UInt(32.W)))
    val fwdRs3Data = Output(Vec(8, UInt(32.W)))
}

class Forward extends Module {
    val io = IO(new ForwardIO)
    
    // 各流水线在EX2、EX3和WB阶段是否可以前递（根据文档表格）
    // EX2前递能力：0:No, 1-7:ALU
    val ex2CanForward = VecInit(Seq(false.B, true.B, true.B, true.B, true.B, true.B, true.B, true.B))
    
    // EX3前递能力：0:No, 1-7:ALU
    val ex3CanForward = VecInit(Seq(false.B, true.B, true.B, true.B, true.B, true.B, true.B, true.B))
    
    // WB前递能力：0-7都可以前递
    val wbCanForward = VecInit(Seq.fill(8)(true.B))
    
    // 为每条流水线的每个源寄存器查找前递数据
    for (i <- 0 until 8) {
        val ex1Pkg = io.ex1Pkgs(i)
        
        // 前3条流水线有3个源寄存器，后面只有2个
        val hasRs3 = i < 3
        
        // ========== 通用前递函数 ==========
        def forwardRs(rsAddr: UInt, rsData: UInt): UInt = {
            val matches = Wire(Vec(24, Bool()))  // 8个EX2 + 8个EX3 + 8个WB
            val data = Wire(Vec(24, UInt(32.W)))

            // 检查EX2阶段的前递（优先级最高）
            for (j <- 0 until 8) {
                val pkg = io.ex2Pkgs(j)
                val isFloat = FloatBypass.isFloatProducer(pkg, j)
                val canFwd = pkg.rdValid && Mux(
                    isFloat,
                    FloatBypass.availableInEx2(pkg, j),
                    ex2CanForward(j)
                )
                val addrMatch = pkg.rd === rsAddr
                matches(j) := canFwd && addrMatch
                data(j) := Mux(isFloat, FloatBypass.ex2Data(pkg), pkg.aluResult)
            }
            
            // 检查EX3阶段的前递（优先级中等）
            for (j <- 0 until 8) {
                val pkg = io.ex3Pkgs(j)
                val isFloat = FloatBypass.isFloatProducer(pkg, j)
                val canFwd = pkg.rdValid && Mux(
                    isFloat,
                    FloatBypass.availableInEx3(pkg, j),
                    ex3CanForward(j)
                )
                val addrMatch = pkg.rd === rsAddr
                matches(j + 8) := canFwd && addrMatch
                data(j + 8) := Mux(isFloat, FloatBypass.ex3Data(pkg, j), pkg.aluResult)
            }
            
            // 检查WB阶段的前递（优先级最低）
            for (j <- 0 until 8) {
                val canFwd = wbCanForward(j) && io.wbPkgs(j).rdValid
                val addrMatch = io.wbPkgs(j).rd === rsAddr
                matches(j + 16) := canFwd && addrMatch
                data(j + 16) := io.wbPkgs(j).rfWdata
            }
            
            // 优先级：EX2 > EX3 > WB，同级中编号大的流水线优先
            // 构建正确的优先级顺序：EX2[7..0], EX3[7..0], WB[7..0]
            val priority = (7 to 0 by -1) ++ (15 to 8 by -1) ++ (23 to 16 by -1)
            MuxCase(rsData, priority.map(j => (matches(j), data(j))))
        }
        
        // ========== 应用前递逻辑 ==========
        io.fwdRs1Data(i) := forwardRs(ex1Pkg.rs1, ex1Pkg.rs1Data)
        io.fwdRs2Data(i) := forwardRs(ex1Pkg.rs2, ex1Pkg.rs2Data)
        io.fwdRs3Data(i) := Mux(hasRs3.B, forwardRs(ex1Pkg.rs3, ex1Pkg.rs3Data), 0.U)
    }
}
