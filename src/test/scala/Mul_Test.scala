import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import scala.util.Random
import ZirconConfig.EXEOp._

class MulTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "MUL"

  private val mask32 = (BigInt(1) << 32) - 1
  private val operations = Seq(MUL, MULH, MULHSU, MULHU).map(_.litValue)

  private def signed32(value: BigInt): BigInt = {
    val bits = value & mask32
    if (bits.testBit(31)) bits - (BigInt(1) << 32) else bits
  }

  private def expected(op: BigInt, src1: BigInt, src2: BigInt): BigInt = {
    val unsigned1 = src1 & mask32
    val unsigned2 = src2 & mask32
    val product = op match {
      case value if value == MUL.litValue || value == MULH.litValue =>
        signed32(unsigned1) * signed32(unsigned2)
      case value if value == MULHSU.litValue =>
        signed32(unsigned1) * unsigned2
      case value if value == MULHU.litValue =>
        unsigned1 * unsigned2
    }

    if (op == MUL.litValue) product & mask32 else (product >> 32) & mask32
  }

  it should "match the software model for edge and random operands" in {
    test(new MulBooth2Wallce).withAnnotations(Seq(VerilatorBackendAnnotation)) { c =>
      c.io.divBusy.poke(false.B)

      val edges = Seq(
        BigInt(0),
        BigInt(1),
        mask32,
        BigInt("80000000", 16),
        BigInt("7fffffff", 16),
        BigInt("55555555", 16),
        BigInt("aaaaaaaa", 16)
      )
      val edgeVectors = for {
        op <- operations
        src1 <- edges
        src2 <- edges
      } yield (op, src1, src2)

      val random = new Random(0x5eed1234L)
      val randomVectors = Seq.fill(10000) {
        val op = operations(random.nextInt(operations.length))
        val src1 = BigInt(java.lang.Integer.toUnsignedLong(random.nextInt()))
        val src2 = BigInt(java.lang.Integer.toUnsignedLong(random.nextInt()))
        (op, src1, src2)
      }

      (edgeVectors ++ randomVectors).foreach { case (op, src1, src2) =>
        c.io.src1.poke(src1.U)
        c.io.src2.poke(src2.U)
        c.io.op.poke(op.U)
        c.clock.step(3)
        c.io.res.expect(
          expected(op, src1, src2).U,
          s"op=0x${op.toString(16)} src1=0x${src1.toString(16)} src2=0x${src2.toString(16)}"
        )
      }
    }
  }
}
