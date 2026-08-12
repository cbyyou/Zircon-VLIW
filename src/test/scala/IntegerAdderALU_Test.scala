import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

import scala.util.Random

class IntegerAdderALUTest extends AnyFlatSpec with ChiselScalatestTester {
  private val mask32 = (BigInt(1) << 32) - 1

  behavior of "BLevelPAdder32"

  it should "match 32-bit addition and carry for edge and random operands" in {
    test(new BLevelPAdder32).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      val random = new Random(0xadd32L)
      val edges = Seq(
        (BigInt(0), BigInt(0), 0),
        (mask32, BigInt(0), 1),
        (mask32, mask32, 1),
        (BigInt("7fffffff", 16), BigInt(1), 0),
        (BigInt("80000000", 16), BigInt("80000000", 16), 0)
      )
      val randomCases = Seq.fill(10000) {
        (BigInt(32, random), BigInt(32, random), random.nextInt(2))
      }

      (edges ++ randomCases).zipWithIndex.foreach { case ((src1, src2, cin), index) =>
        val result = src1 + src2 + cin
        c.io.src1.poke(src1.U)
        c.io.src2.poke(src2.U)
        c.io.cin.poke(cin.U)
        c.io.res.expect((result & mask32).U, s"sum mismatch in case $index")
        c.io.cout.expect(((result >> 32) & 1).U, s"carry mismatch in case $index")
      }
    }
  }

  behavior of "ALU"

  it should "preserve integer arithmetic, logic, shift and compare operations" in {
    test(new ALU).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      val random = new Random(0xa1f00dL)

      def unsigned(value: Int): BigInt = BigInt(value.toLong & 0xffffffffL)
      def check(src1: Int, src2: Int, op: Int, expected: BigInt): Unit = {
        c.io.src1.poke(unsigned(src1).U)
        c.io.src2.poke(unsigned(src2).U)
        c.io.op.poke(op.U)
        c.io.res.expect((expected & mask32).U)
      }

      for (_ <- 0 until 2000) {
        val src1 = random.nextInt()
        val src2 = random.nextInt()
        val shift = src2 & 31
        check(src1, src2, 0x00, unsigned(src1) + unsigned(src2))
        check(src1, src2, 0x08, unsigned(src1) - unsigned(src2))
        check(src1, src2, 0x02, if (src1 < src2) 1 else 0)
        check(src1, src2, 0x03,
          if (unsigned(src1) < unsigned(src2)) 1 else 0)
        check(src1, src2, 0x04, unsigned(src1 ^ src2))
        check(src1, src2, 0x06, unsigned(src1 | src2))
        check(src1, src2, 0x07, unsigned(src1 & src2))
        check(src1, src2, 0x01, unsigned(src1 << shift))
        check(src1, src2, 0x05, unsigned(src1) >> shift)
        check(src1, src2, 0x0d, unsigned(src1 >> shift))
        check(src1, src2, 0x09, unsigned(src2))
        check(src1, src2, 0x1a, unsigned(src1) + 4)
        check(src1, src2, 0x1b, unsigned(src1) + 4)
      }
    }
  }
}
