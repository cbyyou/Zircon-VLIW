import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.CSRCommand._

class FloatingCSRFileTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "FloatingCSRFile"

    it should "read and update frm through frm and fcsr" in {
        test(new FloatingCSRFile) { c =>
            c.io.valid.poke(false.B)
            c.io.address.poke(2.U)
            c.io.command.poke(WRITE)
            c.io.source.poke(0.U)
            c.io.frm.expect(0.U)

            c.io.valid.poke(true.B)
            c.io.source.poke(3.U)
            c.clock.step()
            c.io.valid.poke(false.B)
            c.io.frm.expect(3.U)
            c.io.readData.expect(3.U)

            c.io.address.poke(3.U)
            c.io.valid.poke(true.B)
            c.io.source.poke("h000000a5".U)
            c.clock.step()
            c.io.valid.poke(false.B)
            c.io.frm.expect(5.U)
            c.io.readData.expect("h000000a5".U)

            c.io.address.poke(2.U)
            c.io.command.poke(SET)
            c.io.valid.poke(true.B)
            c.io.source.poke(0.U)
            c.clock.step()
            c.io.valid.poke(false.B)
            c.io.frm.expect(5.U)
        }
    }
}

class DynamicRoundingDecoderTest extends AnyFlatSpec with ChiselScalatestTester {
    behavior of "Decoder dynamic rounding"

    it should "replace DYN with frm and preserve explicit rm" in {
        test(new Decoder(ALU = true, FPU = true, Branch = false, Mem = false, IMulDiv = false, FDiv = true)) { c =>
            c.io.instPkgIn.pc.poke(0.U)
            c.io.instPkgIn.inst.poke(0.U)
            c.io.instPkgIn.rs1.poke(0.U)
            c.io.instPkgIn.rs2.poke(0.U)
            c.io.instPkgIn.rs3.poke(0.U)
            c.io.instPkgIn.rd.poke(0.U)
            c.io.instPkgIn.rdValid.poke(false.B)
            c.io.instPkgIn.rs1Data.poke(0.U)
            c.io.instPkgIn.rs2Data.poke(0.U)
            c.io.instPkgIn.rs3Data.poke(0.U)
            c.io.instPkgIn.op.poke(0.U)
            c.io.instPkgIn.rm.poke(0.U)
            c.io.instPkgIn.imm.poke(0.U)
            c.io.instPkgIn.src1Sel.poke(0.U)
            c.io.instPkgIn.src2Sel.poke(0.U)
            c.io.instPkgIn.aluResult.poke(0.U)
            c.io.instPkgIn.fpuResult.poke(0.U)
            c.io.instPkgIn.fflags.poke(0.U)
            c.io.instPkgIn.branchTgt.poke(0.U)
            c.io.instPkgIn.predFail.poke(false.B)
            c.io.instPkgIn.memResult.poke(0.U)
            c.io.instPkgIn.rfWdata.poke(0.U)
            c.io.frm.poke(3.U)

            val faddDyn = BigInt("00000000001000001111000011010011", 2)
            c.io.instPkgIn.inst.poke(faddDyn.U)
            c.io.instPkgOut.rm.expect(3.U)

            val faddRtz = BigInt("00000000001000001001000011010011", 2)
            c.io.instPkgIn.inst.poke(faddRtz.U)
            c.io.instPkgOut.rm.expect(1.U)
        }
    }
}
