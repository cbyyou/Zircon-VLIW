import chisel3._
import chisel3.util._

object Shifter {
    class ShifterIO(n: Int) extends Bundle{
        val src = Input(UInt(n.W))
        val shf = Input(UInt(log2Ceil(n).W))
        val sgn = Input(Bool())
        val res = Output(UInt(n.W))
    }

    class Shifter extends Module{
        val io = IO(new ShifterIO(32))
        // Five explicit mux stages implement a logarithmic right shifter.
        // SLL continues to reuse this datapath by reversing input and output.
        var shifted = io.src
        for (stage <- 0 until 5) {
            val amount = 1 << stage
            val fill = Fill(amount, io.sgn && io.src(31))
            val shiftedByAmount = Cat(fill, shifted(31, amount))
            shifted = Mux(io.shf(stage), shiftedByAmount, shifted)
        }
        io.res := shifted
    }

    object Shifter {
        def apply(src: UInt, shf: UInt, sgn: Bool): Shifter = {
            val shifter = Module(new Shifter)
            shifter.io.src := src
            shifter.io.shf := shf
            shifter.io.sgn := sgn
            shifter
        }
    }

}
