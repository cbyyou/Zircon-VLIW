import chisel3._
import chisel3.util._
import ZirconConfig.CSRCommand._
import ZirconConfig.FloatingCSRAddress._

class FloatingCSRFileIO extends Bundle {
    val valid = Input(Bool())
    val address = Input(UInt(12.W))
    val command = Input(UInt(2.W))
    val source = Input(UInt(32.W))
    val readData = Output(UInt(32.W))
    val frm = Output(UInt(3.W))
}

class FloatingCSRFile extends Module {
    val io = IO(new FloatingCSRFileIO)

    val fcsr = RegInit(0.U(8.W))

    io.readData := MuxLookup(io.address, 0.U(32.W))(Seq(
        FFLAGS -> Cat(0.U(27.W), fcsr(4, 0)),
        FRM -> Cat(0.U(29.W), fcsr(7, 5)),
        FCSR -> Cat(0.U(24.W), fcsr)
    ))
    io.frm := fcsr(7, 5)

    val updatedValue = MuxLookup(io.command, io.source)(Seq(
        WRITE -> io.source,
        SET -> (io.readData | io.source),
        CLEAR -> (io.readData & ~io.source)
    ))
    val writes = io.command === WRITE || io.source.orR

    when(io.valid && writes) {
        switch(io.address) {
            is(FFLAGS) { fcsr := Cat(fcsr(7, 5), updatedValue(4, 0)) }
            is(FRM) { fcsr := Cat(updatedValue(2, 0), fcsr(4, 0)) }
            is(FCSR) { fcsr := updatedValue(7, 0) }
        }
    }
}
