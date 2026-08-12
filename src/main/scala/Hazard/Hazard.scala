import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._

class HazardIO extends Bundle {
    val frontend = Flipped(new FrontendHazardIO)
    val backend = Flipped(new BackendHazardIO)
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
    
    // ========== 1. 除法器停顿处理（最高优先级）==========
    // 包括整数除法器（流水线3、4）和浮点除法器（流水线0）
    val intDivStall = io.backend.divBusy(0) || io.backend.divBusy(1)
    val fpDivStall = io.backend.fdivBusy
    val divStall = intDivStall || fpDivStall
    when(divStall) {
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
    when(!divStall && io.backend.predFail) {
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
    
    // ========== 3. RAW数据相关处理 ==========
    // A consumer enters EX1 one cycle after this check. It only stalls until
    // the producer will be visible on the matching EX2/EX3/WB bypass path.
    def isFloatAdd(op: UInt): Bool = {
        op === FADD_S || op === FSUB_S
    }

    def isFloatConvert(op: UInt): Bool = {
        op === FCVT_W_S || op === FCVT_WU_S || op === FCVT_S_W || op === FCVT_S_WU
    }

    def isFloatPipelineOp(op: UInt, pipelineIdx: Int): Bool = {
        if (pipelineIdx <= 2) op(6) else false.B
    }

    def isLoad(op: UInt, pipelineIdx: Int): Bool = {
        ((pipelineIdx == 5) || (pipelineIdx == 6)).B && op(4) && !op(5)
    }

    def isMulDiv(op: UInt, pipelineIdx: Int): Bool = {
        ((pipelineIdx == 3) || (pipelineIdx == 4)).B && op(4)
    }

    def isIntegerDiv(op: UInt, pipelineIdx: Int): Bool = {
        isMulDiv(op, pipelineIdx) && op(2)
    }

    def isIntegerMul(op: UInt, pipelineIdx: Int): Bool = {
        isMulDiv(op, pipelineIdx) && !op(2)
    }

    def multiplyFeedsControl(pkg: InstructionPackage, rd: UInt): Bool = {
        val isConditional = pkg.op === BEQ || pkg.op === BNE || pkg.op === BLT ||
            pkg.op === BGE || pkg.op === BLTU || pkg.op === BGEU
        (isConditional && (pkg.rs1 === rd || pkg.rs2 === rd)) ||
            (pkg.op === JALR && pkg.rs1 === rd)
    }

    def floatResultAtEx3(op: UInt, pipelineIdx: Int): Bool = {
        isFloatPipelineOp(op, pipelineIdx) && (
            isFloatConvert(op) || op === FDIV_S || op === FSQRT_S
        )
    }

    def stallForEx1Producer(op: UInt, pipelineIdx: Int): Bool = {
        val floatNeedsLaterStage = floatResultAtEx3(op, pipelineIdx) ||
            (isFloatPipelineOp(op, pipelineIdx) && (isFloatAdd(op) || op === FMUL_S))
        isLoad(op, pipelineIdx) || isMulDiv(op, pipelineIdx) || floatNeedsLaterStage
    }

    def stallForEx2Producer(op: UInt, pipelineIdx: Int): Bool = {
        val floatNeedsWB = isFloatPipelineOp(op, pipelineIdx) && (isFloatAdd(op) || op === FMUL_S)
        isLoad(op, pipelineIdx) || isIntegerDiv(op, pipelineIdx) || floatNeedsWB
    }
    
    // 检查RAW冲突：ID阶段的指令依赖EX1或EX2阶段的指令
    val rawHazard = Wire(Bool())
    rawHazard := false.B
    
    for (idIdx <- 0 until 8) {  // ID阶段的8条指令
        val idPkg = io.frontend.idPkgs(idIdx)
        
        // 检查与EX1阶段的冲突
        for (ex1Idx <- 0 until 8) {
            val ex1Pkg = io.backend.ex1Pkgs(ex1Idx)
            when(ex1Pkg.rdValid && stallForEx1Producer(ex1Pkg.op, ex1Idx)) {
                // 检查rs1, rs2, rs3是否与rd相关
                val rs1Match = idPkg.rs1 === ex1Pkg.rd
                val rs2Match = idPkg.rs2 === ex1Pkg.rd
                val rs3Match = (idIdx < 3).B && (idPkg.rs3 === ex1Pkg.rd)  // 只有前3条流水线有rs3
                when(rs1Match || rs2Match || rs3Match) {
                    rawHazard := true.B
                }
            }
        }
        
        // 检查与EX2阶段的冲突
        for (ex2Idx <- 0 until 8) {
            val ex2Pkg = io.backend.ex2Pkgs(ex2Idx)
            when(ex2Pkg.rdValid && stallForEx2Producer(ex2Pkg.op, ex2Idx)) {
                val rs1Match = idPkg.rs1 === ex2Pkg.rd
                val rs2Match = idPkg.rs2 === ex2Pkg.rd
                val rs3Match = (idIdx < 3).B && (idPkg.rs3 === ex2Pkg.rd)
                when(rs1Match || rs2Match || rs3Match) {
                    rawHazard := true.B
                }
            }
            if (idIdx == 7) {
                when(ex2Pkg.rdValid && isIntegerMul(ex2Pkg.op, ex2Idx) &&
                     multiplyFeedsControl(idPkg, ex2Pkg.rd)) {
                    rawHazard := true.B
                }
            }
        }
        // Floating-point EX3 producers are available either directly from EX3
        // or from WB in the consumer's following EX1 cycle. Keep the existing
        // conservative policy for load and variable-latency integer divide.
        // Integer multiply can leave EX3: the consumer reaches EX1 while the
        // producer is still available on the existing WB bypass.
        for (ex3Idx <- 0 until 8) {
            val ex3Pkg = io.backend.ex3Pkgs(ex3Idx)
            val nonFloatNeedsWB = isLoad(ex3Pkg.op, ex3Idx) ||
                isIntegerDiv(ex3Pkg.op, ex3Idx)
            when(ex3Pkg.rdValid && nonFloatNeedsWB) {
                val rs1Match = idPkg.rs1 === ex3Pkg.rd
                val rs2Match = idPkg.rs2 === ex3Pkg.rd
                val rs3Match = (idIdx < 3).B && (idPkg.rs3 === ex3Pkg.rd)
                when(rs1Match || rs2Match || rs3Match) {
                    rawHazard := true.B
                }
            }
        }
    }
    
    // RAW冲突处理：如果没有除法器停顿和分支冲刷，则处理RAW冲突
    // 注意：分支冲刷优先于RAW stall，否则PC无法更新到正确的跳转地址
    when(!divStall && !io.backend.predFail && rawHazard) {
        // 对前端发起停顿
        io.frontend.stall := true.B
        // 冲刷ID-EX1寄存器
        for (i <- 0 until 8) {
            io.backend.ex1Flush(i) := true.B
        }
    }

}
