import chisel3._
import circt.stage.ChiselStage
import chisel3.stage.ChiselOption

object GenerateCPU {
    private val firtoolOptions = Array(
        "-disable-all-randomization", 
        "-strip-debug-info",
        "-strip-fir-debug-info",
        "-O=release",
        "--ignore-read-enable-mem",
        "--lower-memories",
        "--lowering-options=noAlwaysComb, disallowPackedArrays, disallowLocalVariables, explicitBitcast, disallowMuxInlining, disallowExpressionInliningInPorts",
        "-o=verilog/",
        "-split-verilog",
    )

    def apply(enablePerfCounters: Boolean): Unit = {
        val isSim = Option(System.getenv("BUILD_MODE")).getOrElse("SYNC") != "SYNC"
        println(s"isSim: $isSim, enablePerfCounters: $enablePerfCounters")
        ChiselStage.emitSystemVerilogFile(
            new CPU(enablePerfCounters),
            Array("-td", "build/"),
            firtoolOpts = firtoolOptions,
        )
    }
}

object Main extends App {
    GenerateCPU(enablePerfCounters = false)
}

object PerfDebugMain extends App {
    GenerateCPU(enablePerfCounters = true)
}
