import chisel3._
import ZirconConfig.EXEOp._

object FloatBypass {
    private def isFpuLane(pipelineIdx: Int): Bool = (pipelineIdx <= 2).B
    private def isLsuLane(pipelineIdx: Int): Bool = (pipelineIdx == 5 || pipelineIdx == 6).B

    def isFloatLoad(pkg: InstructionPackage, pipelineIdx: Int): Bool = {
        isLsuLane(pipelineIdx) && pkg.op === FLW && pkg.rd(5)
    }

    def isFloatProducer(pkg: InstructionPackage, pipelineIdx: Int): Bool = {
        pkg.rdValid && ((isFpuLane(pipelineIdx) && pkg.op(6)) || isFloatLoad(pkg, pipelineIdx))
    }

    private def isFAdd(pkg: InstructionPackage): Bool = {
        pkg.op === FADD_S || pkg.op === FSUB_S
    }

    private def isFMul(pkg: InstructionPackage): Bool = pkg.op === FMUL_S

    private def isFDiv(pkg: InstructionPackage): Bool = {
        pkg.op === FDIV_S || pkg.op === FSQRT_S
    }

    private def isImmediateFpu(pkg: InstructionPackage): Bool = {
        Seq(
            FSGNJ_S,
            FSGNJN_S,
            FSGNJX_S,
            FMIN_S,
            FMAX_S,
            FLE_S,
            FLT_S,
            FEQ_S,
            FCVT_W_S,
            FCVT_WU_S,
            FCVT_S_W,
            FCVT_S_WU,
            FMV_X_W,
            FCLASS_S,
            FMV_W_X
        ).map(pkg.op === _).reduce(_ || _)
    }

    def availableInEx2(pkg: InstructionPackage, pipelineIdx: Int): Bool = {
        isFloatProducer(pkg, pipelineIdx) && isFpuLane(pipelineIdx) && isImmediateFpu(pkg)
    }

    def availableInEx3(pkg: InstructionPackage, pipelineIdx: Int): Bool = {
        val fpuResultReady = isFpuLane(pipelineIdx) && (isImmediateFpu(pkg) || isFAdd(pkg) || isFDiv(pkg))
        isFloatProducer(pkg, pipelineIdx) && (fpuResultReady || isFloatLoad(pkg, pipelineIdx))
    }

    def ex2Data(pkg: InstructionPackage): UInt = pkg.fpuResult

    def ex3Data(pkg: InstructionPackage, pipelineIdx: Int): UInt = {
        Mux(isFloatLoad(pkg, pipelineIdx), pkg.memResult, pkg.fpuResult)
    }
}
