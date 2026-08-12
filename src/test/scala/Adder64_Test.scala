import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec

class Adder64Test extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "BLevelPAdder64"

  private val mask64 = (BigInt(1) << 64) - 1

  private def check(c: BLevelPAdder64, src1: BigInt, src2: BigInt, cin: Int): Unit = {
    val expected = src1 + src2 + cin
    c.io.src1.poke((src1 & mask64).U)
    c.io.src2.poke((src2 & mask64).U)
    c.io.cin.poke(cin.U)
    c.io.res.expect((expected & mask64).U)
    c.io.cout.expect(((expected >> 64) & 1).U)
  }

  it should "match unsigned 65-bit addition for edge and random inputs" in {
    test(new BLevelPAdder64).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      val edges = Seq(
        BigInt(0),
        BigInt(1),
        (BigInt(1) << 32) - 1,
        BigInt(1) << 32,
        BigInt(1) << 63,
        mask64
      )
      for {
        src1 <- edges
        src2 <- edges
        cin <- Seq(0, 1)
      } {
        check(c, src1, src2, cin)
      }

      val random = new scala.util.Random(0x64add)
      for (_ <- 0 until 10000) {
        check(c, BigInt(64, random), BigInt(64, random), random.nextInt(2))
      }
    }
  }

  it should "keep the split prefix result aligned across stalls" in {
    test(new PipelinedPrefixAdder64(preLevels = 3))
      .withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
        val vectors = Seq(
          (BigInt(0), BigInt(0), 0),
          (mask64, BigInt(1), 0),
          (BigInt("fedcba9876543210", 16), BigInt("123456789abcdef0", 16), 1)
        )

        c.io.enable.poke(true.B)
        for ((src1, src2, cin) <- vectors) {
          c.io.src1.poke(src1.U)
          c.io.src2.poke(src2.U)
          c.io.cin.poke(cin.U)
          c.clock.step()
          val expected = src1 + src2 + cin
          c.io.res.expect((expected & mask64).U)
          c.io.cout.expect(((expected >> 64) & 1).U)
        }

        c.io.enable.poke(false.B)
        c.io.src1.poke("h1111111111111111".U)
        c.io.src2.poke("h2222222222222222".U)
        c.io.cin.poke(0.U)
        c.clock.step(3)
        val held = vectors.last._1 + vectors.last._2 + vectors.last._3
        c.io.res.expect((held & mask64).U)
        c.io.cout.expect(((held >> 64) & 1).U)
      }
  }
}
