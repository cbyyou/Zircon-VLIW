import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._

class HazardIO extends Bundle {
    val frontend = Flipped(new FrontendHazardIO)
    val backend = Flipped(new BackendHazardIO)
    val debug = Output(new HazardDebugIO)
}

class HazardDebugIO extends Bundle {
    val rawStall = Bool()
    val rawLoad = Bool()
    val rawIntegerMul = Bool()
    val rawFloatAdd = Bool()
    val rawFloatMul = Bool()
    val rawFloatDiv = Bool()
    val falseRawStall = Bool()
    val branchRedirect = Bool()
    val executionStall = Bool()
    val floatDivStall = Bool()
    val integerDivStall = Bool()
    val integerDivStarts = UInt(2.W)
    val integerDivStartsOnRedirect = UInt(2.W)
}

class Hazard extends Module {
    val io = IO(new HazardIO)
    
    // ========== 默认不产生任何控制信号 ==========
    io.frontend.flush := false.B
    io.frontend.stall := false.B
    for (i <- 0 until 8) {
        io.backend.ex1Flush(i) := false.B
        io.backend.ex1Stall(i) := false.B
        io.backend.ex2Flush(i) := false.B
        io.backend.ex2Stall(i) := false.B
        io.backend.ex3Flush(i) := false.B
        io.backend.ex3Stall(i) := false.B
        io.backend.wbFlush(i) := false.B
        io.backend.wbStall(i) := false.B
    }
    
    // ========== 1. 全局停顿处理（最高优先级）==========
    // 任意一条流水线阻塞时，所有8条流水线保持同步。
    val executionStall = io.backend.pipelineBusy.asUInt.orR
    val globalStall = executionStall || io.backend.memBusy
    val executionStarting = io.backend.pipelineStart.asUInt.orR
    when(globalStall) {
        // 对前端发起停顿
        io.frontend.stall := true.B
        // 停顿所有的ID-EX1、EX1-EX2、EX2-EX3寄存器
        for (i <- 0 until 8) {
            io.backend.ex1Stall(i) := true.B
            io.backend.ex2Stall(i) := true.B
            io.backend.ex3Stall(i) := true.B
        }
        // 冲刷所有的EX3-WB寄存器
        for (i <- 0 until 8) {
            io.backend.wbFlush(i) := true.B
        }
    }
    
    // ========== 2. 分支预测失败处理（次优先级）==========
    when(!globalStall && io.backend.predFail) {
        // 给前端flush信号
        io.frontend.flush := true.B
        // 给ID-EX1段间寄存器flush信号
        for (i <- 0 until 8) {
            io.backend.ex1Flush(i) := true.B
        }
        // 给EX1-EX2段间寄存器flush信号（Branch在EX1阶段，所以EX1的指令也要flush）
        for (i <- 0 until 8) {
            io.backend.ex2Flush(i) := true.B
        }
    }

    // A variable-latency request may not assert busy until the next cycle.
    // Let the issuing packet advance, but keep the younger ID packet out of EX1.
    when(!globalStall && !io.backend.predFail && executionStarting) {
        io.frontend.stall := true.B
        for (i <- 0 until 8) {
            io.backend.ex1Flush(i) := true.B
        }
    }
    
    // ========== 3. RAW数据相关处理 ==========
    // 判断指令是否需要WB阶段才能访问（load/mul/div/float）
    def needWB(op: UInt, pipelineIdx: Int): Bool = {
        val isLoad = op(4) && !op(5)  // op[4]=1表示branch/load/muldiv，但load没有op[5]
        val isMulDiv = op(4) && ((pipelineIdx == 3) || (pipelineIdx == 4)).B  // 乘除法在流水线3-4
        val isFloat = op(6)  // op[6]=1表示float（不包括fdiv）
        // FDiv 需要根据操作码判断，而不是流水线编号
        val isFDiv = (op === ZirconConfig.EXEOp.FDIV_S) || (op === ZirconConfig.EXEOp.FSQRT_S)
        val isCSR = op === CSRRW || op === CSRRS || op === CSRRC
        isLoad || isMulDiv || isFloat || isFDiv || isCSR
    }

    def blocksConsumerInEx1(pkg: InstructionPackage, pipelineIdx: Int): Bool = {
        val isFloat = FloatBypass.isFloatProducer(pkg, pipelineIdx)
        Mux(isFloat, !FloatBypass.availableInEx2(pkg, pipelineIdx), needWB(pkg.op, pipelineIdx))
    }

    def blocksConsumerInEx2(pkg: InstructionPackage, pipelineIdx: Int): Bool = {
        val isFloat = FloatBypass.isFloatProducer(pkg, pipelineIdx)
        Mux(isFloat, !FloatBypass.availableInEx3(pkg, pipelineIdx), needWB(pkg.op, pipelineIdx))
    }
    
    // 检查RAW冲突：ID阶段的指令依赖EX1或EX2阶段的指令
    val rawHazard = Wire(Bool())
    val semanticRawHazard = Wire(Bool())
    val rawLoadHazard = Wire(Bool())
    val rawIntegerMulHazard = Wire(Bool())
    val rawFloatAddHazard = Wire(Bool())
    val rawFloatMulHazard = Wire(Bool())
    val rawFloatDivHazard = Wire(Bool())
    rawHazard := false.B
    semanticRawHazard := false.B
    rawLoadHazard := false.B
    rawIntegerMulHazard := false.B
    rawFloatAddHazard := false.B
    rawFloatMulHazard := false.B
    rawFloatDivHazard := false.B

    def markRawProducer(pkg: InstructionPackage, pipelineIdx: Int): Unit = {
        val isLoad = ((pipelineIdx == 5) || (pipelineIdx == 6)).B && pkg.op(4) && !pkg.op(5)
        val isIntegerMul = ((pipelineIdx == 3) || (pipelineIdx == 4)).B && Seq(
            MUL, MULH, MULHSU, MULHU
        ).map(pkg.op === _).reduce(_ || _)
        when(isLoad) {
            rawLoadHazard := true.B
        }
        when(isIntegerMul) {
            rawIntegerMulHazard := true.B
        }
        when(pkg.op === FADD_S || pkg.op === FSUB_S) {
            rawFloatAddHazard := true.B
        }
        when(pkg.op === FMUL_S) {
            rawFloatMulHazard := true.B
        }
        when(pkg.op === FDIV_S || pkg.op === FSQRT_S) {
            rawFloatDivHazard := true.B
        }
    }

    def usesRs1(pkg: InstructionPackage): Bool = {
        val opcode = pkg.inst(6, 0)
        val regularRs1 = Seq(
            "h67".U, "h63".U, "h03".U, "h07".U, "h23".U, "h27".U,
            "h13".U, "h33".U, "h53".U, "h43".U, "h47".U, "h4b".U, "h4f".U
        ).map(opcode === _).reduce(_ || _)
        val registerCSR = opcode === "h73".U && !pkg.inst(14)
        regularRs1 || registerCSR
    }

    def usesRs2(pkg: InstructionPackage): Bool = {
        val opcode = pkg.inst(6, 0)
        val integerRs2 = Seq("h63".U, "h23".U, "h27".U, "h33".U)
            .map(opcode === _).reduce(_ || _)
        val floatRs2 = Seq(
            FADD_S, FSUB_S, FMUL_S, FDIV_S,
            FSGNJ_S, FSGNJN_S, FSGNJX_S, FMIN_S, FMAX_S,
            FLE_S, FLT_S, FEQ_S
        ).map(pkg.op === _).reduce(_ || _)
        val fusedRs2 = Seq("h43".U, "h47".U, "h4b".U, "h4f".U)
            .map(opcode === _).reduce(_ || _)
        integerRs2 || floatRs2 || fusedRs2
    }

    def usesRs3(pkg: InstructionPackage): Bool = {
        val opcode = pkg.inst(6, 0)
        Seq("h43".U, "h47".U, "h4b".U, "h4f".U)
            .map(opcode === _).reduce(_ || _)
    }
    
    for (idIdx <- 0 until 8) {  // ID阶段的8条指令
        val idPkg = io.frontend.idPkgs(idIdx)
        
        // 检查与EX1阶段的冲突
        for (ex1Idx <- 0 until 8) {
            val ex1Pkg = io.backend.ex1Pkgs(ex1Idx)
            when(ex1Pkg.rdValid && blocksConsumerInEx1(ex1Pkg, ex1Idx)) {
                // 检查rs1, rs2, rs3是否与rd相关
                val rs1Match = idPkg.rs1 === ex1Pkg.rd
                val rs2Match = idPkg.rs2 === ex1Pkg.rd
                val rs3Match = (idIdx < 3).B && (idPkg.rs3 === ex1Pkg.rd)  // 只有前3条流水线有rs3
                val usedSourceMatch =
                    (usesRs1(idPkg) && rs1Match) ||
                    (usesRs2(idPkg) && rs2Match) ||
                    (usesRs3(idPkg) && rs3Match)
                when(usedSourceMatch) {
                    rawHazard := true.B
                    semanticRawHazard := true.B
                    markRawProducer(ex1Pkg, ex1Idx)
                }
            }
        }
        
        // 检查与EX2阶段的冲突
        for (ex2Idx <- 0 until 8) {
            val ex2Pkg = io.backend.ex2Pkgs(ex2Idx)
            when(ex2Pkg.rdValid && blocksConsumerInEx2(ex2Pkg, ex2Idx)) {
                val rs1Match = idPkg.rs1 === ex2Pkg.rd
                val rs2Match = idPkg.rs2 === ex2Pkg.rd
                val rs3Match = (idIdx < 3).B && (idPkg.rs3 === ex2Pkg.rd)
                val usedSourceMatch =
                    (usesRs1(idPkg) && rs1Match) ||
                    (usesRs2(idPkg) && rs2Match) ||
                    (usesRs3(idPkg) && rs3Match)
                when(usedSourceMatch) {
                    rawHazard := true.B
                    semanticRawHazard := true.B
                    markRawProducer(ex2Pkg, ex2Idx)
                }
            }
        }
    }

    def usesDynamicRounding(pkg: InstructionPackage): Bool = {
        val roundingOp = Seq(
            FADD_S, FSUB_S, FMUL_S, FDIV_S, FSQRT_S,
            FCVT_W_S, FCVT_WU_S, FCVT_S_W, FCVT_S_WU,
            FMADD_S, FMSUB_S, FNMSUB_S, FNMADD_S
        ).map(pkg.op === _).reduce(_ || _)
        pkg.inst =/= 0.U && roundingOp && pkg.inst(14, 12) === "b111".U
    }

    val frmHazard = io.backend.frmWritePending &&
        io.frontend.idPkgs.map(usesDynamicRounding).reduce(_ || _)
    val dependencyHazard = rawHazard || frmHazard
    
    // RAW冲突处理：如果没有全局停顿和分支冲刷，则处理RAW冲突
    // 注意：分支冲刷优先于RAW stall，否则PC无法更新到正确的跳转地址
    when(!globalStall && !io.backend.predFail && !executionStarting && dependencyHazard) {
        // 对前端发起停顿
        io.frontend.stall := true.B
        // 冲刷ID-EX1寄存器
        for (i <- 0 until 8) {
            io.backend.ex1Flush(i) := true.B
        }
    }

    val selectedExecutionStart = !globalStall && !io.backend.predFail && executionStarting
    val selectedRawStall = !globalStall && !io.backend.predFail && !executionStarting && rawHazard
    val selectedBranchRedirect = !globalStall && io.backend.predFail
    val selectedExecutionStall = executionStall || selectedExecutionStart

    io.debug.rawStall := selectedRawStall
    io.debug.rawLoad := selectedRawStall && rawLoadHazard
    io.debug.rawIntegerMul := selectedRawStall && rawIntegerMulHazard
    io.debug.rawFloatAdd := selectedRawStall && rawFloatAddHazard
    io.debug.rawFloatMul := selectedRawStall && rawFloatMulHazard
    io.debug.rawFloatDiv := selectedRawStall && rawFloatDivHazard
    io.debug.falseRawStall := selectedRawStall && !semanticRawHazard
    io.debug.branchRedirect := selectedBranchRedirect
    io.debug.executionStall := selectedExecutionStall
    io.debug.floatDivStall := selectedExecutionStall &&
        (io.backend.pipelineBusy(0) || io.backend.pipelineStart(0))
    io.debug.integerDivStall := selectedExecutionStall && (
        io.backend.pipelineBusy(3) || io.backend.pipelineBusy(4) ||
        io.backend.pipelineStart(3) || io.backend.pipelineStart(4)
    )
    io.debug.integerDivStarts := PopCount(VecInit(Seq(
        io.backend.pipelineStart(3), io.backend.pipelineStart(4)
    )))
    io.debug.integerDivStartsOnRedirect := PopCount(VecInit(Seq(
        io.backend.pipelineStart(3) && io.backend.predFail,
        io.backend.pipelineStart(4) && io.backend.predFail
    )))
}
