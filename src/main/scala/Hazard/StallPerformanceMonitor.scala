import chisel3._
import chisel3.util._

class HazardEventIO extends Bundle {
    val rawStall = Bool()
    val loadRaw = Bool()
    val integerRaw = Bool()
    val floatRaw = Bool()

    val intDivBusy = Bool()
    val fdivBusy = Bool()
    val fsqrtBusy = Bool()

    val branchResolved = Bool()
    val branchTaken = Bool()
    val branchMispredict = Bool()
    val branchFlush = Bool()
}

class StallPerformanceCounterValues extends Bundle {
    val cycleCount = UInt(64.W)
    val totalStallCycles = UInt(64.W)
    val rawStallCycles = UInt(64.W)
    val loadRawStallCycles = UInt(64.W)
    val integerRawStallCycles = UInt(64.W)
    val floatRawStallCycles = UInt(64.W)
    val mixedRawStallCycles = UInt(64.W)
    val intDivOnlyStallCycles = UInt(64.W)
    val fdivOnlyStallCycles = UInt(64.W)
    val fsqrtOnlyStallCycles = UInt(64.W)
    val longLatencyOverlapStallCycles = UInt(64.W)
    val branchCount = UInt(64.W)
    val branchTakenCount = UInt(64.W)
    val branchMispredictCount = UInt(64.W)
    val branchFlushCycles = UInt(64.W)
}

class StallPerformanceMonitorIO extends Bundle {
    val events = Input(new HazardEventIO)
    val counters = Output(new StallPerformanceCounterValues)
}

class StallPerformanceMonitor extends Module {
    val io = IO(new StallPerformanceMonitorIO)

    val cycleCount = RegInit(0.U(64.W))
    val totalStallCycles = RegInit(0.U(64.W))
    val rawStallCycles = RegInit(0.U(64.W))
    val loadRawStallCycles = RegInit(0.U(64.W))
    val integerRawStallCycles = RegInit(0.U(64.W))
    val floatRawStallCycles = RegInit(0.U(64.W))
    val mixedRawStallCycles = RegInit(0.U(64.W))
    val intDivOnlyStallCycles = RegInit(0.U(64.W))
    val fdivOnlyStallCycles = RegInit(0.U(64.W))
    val fsqrtOnlyStallCycles = RegInit(0.U(64.W))
    val longLatencyOverlapStallCycles = RegInit(0.U(64.W))
    val branchCount = RegInit(0.U(64.W))
    val branchTakenCount = RegInit(0.U(64.W))
    val branchMispredictCount = RegInit(0.U(64.W))
    val branchFlushCycles = RegInit(0.U(64.W))

    val rawClassCount = PopCount(Seq(
        io.events.loadRaw,
        io.events.integerRaw,
        io.events.floatRaw
    ))
    val mixedRawStall = io.events.rawStall && rawClassCount > 1.U
    val loadRawStall = io.events.rawStall && rawClassCount === 1.U && io.events.loadRaw
    val integerRawStall = io.events.rawStall && rawClassCount === 1.U && io.events.integerRaw
    val floatRawStall = io.events.rawStall && rawClassCount === 1.U && io.events.floatRaw

    val longLatencyClassCount = PopCount(Seq(
        io.events.intDivBusy,
        io.events.fdivBusy,
        io.events.fsqrtBusy
    ))
    val longLatencyStall = longLatencyClassCount =/= 0.U
    val longLatencyOverlapStall = longLatencyClassCount > 1.U
    val intDivOnlyStall = longLatencyClassCount === 1.U && io.events.intDivBusy
    val fdivOnlyStall = longLatencyClassCount === 1.U && io.events.fdivBusy
    val fsqrtOnlyStall = longLatencyClassCount === 1.U && io.events.fsqrtBusy

    cycleCount := cycleCount + 1.U
    when(io.events.rawStall || longLatencyStall) {
        totalStallCycles := totalStallCycles + 1.U
    }
    when(io.events.rawStall) {
        rawStallCycles := rawStallCycles + 1.U
    }
    when(loadRawStall) {
        loadRawStallCycles := loadRawStallCycles + 1.U
    }
    when(integerRawStall) {
        integerRawStallCycles := integerRawStallCycles + 1.U
    }
    when(floatRawStall) {
        floatRawStallCycles := floatRawStallCycles + 1.U
    }
    when(mixedRawStall) {
        mixedRawStallCycles := mixedRawStallCycles + 1.U
    }
    when(intDivOnlyStall) {
        intDivOnlyStallCycles := intDivOnlyStallCycles + 1.U
    }
    when(fdivOnlyStall) {
        fdivOnlyStallCycles := fdivOnlyStallCycles + 1.U
    }
    when(fsqrtOnlyStall) {
        fsqrtOnlyStallCycles := fsqrtOnlyStallCycles + 1.U
    }
    when(longLatencyOverlapStall) {
        longLatencyOverlapStallCycles := longLatencyOverlapStallCycles + 1.U
    }
    when(io.events.branchResolved) {
        branchCount := branchCount + 1.U
    }
    when(io.events.branchResolved && io.events.branchTaken) {
        branchTakenCount := branchTakenCount + 1.U
    }
    when(io.events.branchResolved && io.events.branchMispredict) {
        branchMispredictCount := branchMispredictCount + 1.U
    }
    when(io.events.branchFlush) {
        branchFlushCycles := branchFlushCycles + 1.U
    }

    io.counters.cycleCount := cycleCount
    io.counters.totalStallCycles := totalStallCycles
    io.counters.rawStallCycles := rawStallCycles
    io.counters.loadRawStallCycles := loadRawStallCycles
    io.counters.integerRawStallCycles := integerRawStallCycles
    io.counters.floatRawStallCycles := floatRawStallCycles
    io.counters.mixedRawStallCycles := mixedRawStallCycles
    io.counters.intDivOnlyStallCycles := intDivOnlyStallCycles
    io.counters.fdivOnlyStallCycles := fdivOnlyStallCycles
    io.counters.fsqrtOnlyStallCycles := fsqrtOnlyStallCycles
    io.counters.longLatencyOverlapStallCycles := longLatencyOverlapStallCycles
    io.counters.branchCount := branchCount
    io.counters.branchTakenCount := branchTakenCount
    io.counters.branchMispredictCount := branchMispredictCount
    io.counters.branchFlushCycles := branchFlushCycles
}
