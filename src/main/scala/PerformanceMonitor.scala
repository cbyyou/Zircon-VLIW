import chisel3._
import chisel3.util._

class PerformanceMonitorIO extends Bundle {
    val wbValid = Input(Vec(8, Bool()))
    val wbInst = Input(Vec(8, UInt(32.W)))

    val cycles = Output(UInt(64.W))
    val effectiveInstructions = Output(UInt(64.W))
    val executedPackets = Output(UInt(64.W))
}

class PerformanceMonitor extends Module {
    val io = IO(new PerformanceMonitorIO)

    val nopInstruction = "h00000013".U(32.W)
    val fillerFeqInstruction = "ha0002053".U(32.W)
    val haltInstruction = "h80000000".U(32.W)

    val cycleCount = RegInit(0.U(64.W))
    val effectiveInstructionCount = RegInit(0.U(64.W))
    val executedPacketCount = RegInit(0.U(64.W))

    val effectiveCommits = VecInit((0 until 8).map { i =>
        io.wbValid(i) &&
        io.wbInst(i) =/= nopInstruction &&
        io.wbInst(i) =/= fillerFeqInstruction &&
        io.wbInst(i) =/= haltInstruction
    })
    val packetCommitted = io.wbValid.asUInt.orR
    val packetContainsHalt = VecInit((0 until 8).map { i =>
        io.wbValid(i) && io.wbInst(i) === haltInstruction
    }).asUInt.orR

    cycleCount := cycleCount + 1.U
    effectiveInstructionCount := effectiveInstructionCount + PopCount(effectiveCommits)
    when(packetCommitted && !packetContainsHalt) {
        executedPacketCount := executedPacketCount + 1.U
    }

    io.cycles := cycleCount
    io.effectiveInstructions := effectiveInstructionCount
    io.executedPackets := executedPacketCount
}
