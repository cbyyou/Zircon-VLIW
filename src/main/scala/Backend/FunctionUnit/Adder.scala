import chisel3._
import chisel3.util._

// select a kind of Adder
class AdderIO(n: Int) extends Bundle{
    val src1 = Input(UInt(n.W))
    val src2 = Input(UInt(n.W))
    val cin  = Input(UInt(1.W))
    val res  = Output(UInt(n.W))
    val cout = Output(UInt(1.W))
}

object BLevelCarry4 {
    def apply(p: UInt, g: UInt, c: UInt): (UInt, UInt, UInt) = {
        assert(p.getWidth == 4, "p must be 4 bits wide")
        assert(g.getWidth == 4, "g must be 4 bits wide")
        assert(c.getWidth == 1, "c must be 1 bits wide")
        val pn = p.andR
        val gn = g(3) | (p(3) & g(2)) | (p(3) & p(2) & g(1)) | (p(3) & p(2) & p(1) & g(0))
        val cn = Wire(Vec(4, UInt(1.W)))
        cn(0) := g(0) | (p(0) & c)
        cn(1) := g(1) | (p(1) & g(0)) | (p(1) & p(0) & c)
        cn(2) := g(2) | (p(2) & g(1)) | (p(2) & p(1) & g(0)) | (p(2) & p(1) & p(0) & c)
        cn(3) := gn | (pn & c)
        (pn, gn, cn.asUInt)
    }
}

class BLevelAdder4 extends Module {
    val io = IO(new AdderIO(4))
    val (p, g, c) = BLevelCarry4(io.src1, io.src2, io.cin)
    io.res := io.src1 ^ io.src2 ^ (c.asUInt(3, 0) ## io.cin)
    io.cout := c.asUInt(3)
}

class BLevelAdder5 extends Module {
    val io = IO(new AdderIO(5))
    val adder4 = BLevelAdder4(io.src1(4, 0), io.src2(4, 0), io.cin)
    io.res := (adder4.io.cout ^ io.src1(4) ^ io.src2(4)) ## adder4.io.res
    io.cout := (adder4.io.cout & io.src1(4)) | (adder4.io.cout & io.src2(4)) | (io.src1(4) & io.src2(4))
}


class BLevelPAdder32 extends Module{
    val io = IO(new AdderIO(32))
    val bitPropagate = Wire(Vec(32, Bool()))
    val bitGenerate = Wire(Vec(32, Bool()))
    for (i <- 0 until 32) {
        bitPropagate(i) := io.src1(i) ^ io.src2(i)
        bitGenerate(i) := io.src1(i) & io.src2(i)
    }

    // Five prefix levels cover 1, 2, 4, 8 and 16 lower bits.
    var levelPropagate: Vec[Bool] = bitPropagate
    var levelGenerate: Vec[Bool] = bitGenerate
    var distance = 1
    while (distance < 32) {
        val nextPropagate = Wire(Vec(32, Bool()))
        val nextGenerate = Wire(Vec(32, Bool()))
        for (i <- 0 until 32) {
            if (i >= distance) {
                nextGenerate(i) := levelGenerate(i) |
                    (levelPropagate(i) & levelGenerate(i - distance))
                nextPropagate(i) := levelPropagate(i) & levelPropagate(i - distance)
            } else {
                nextGenerate(i) := levelGenerate(i)
                nextPropagate(i) := levelPropagate(i)
            }
        }
        levelPropagate = nextPropagate
        levelGenerate = nextGenerate
        distance = distance << 1
    }

    val carry = Wire(Vec(33, Bool()))
    carry(0) := io.cin(0)
    for (i <- 0 until 32) {
        carry(i + 1) := levelGenerate(i) | (levelPropagate(i) & io.cin(0))
    }

    io.res := VecInit((0 until 32).map(i => bitPropagate(i) ^ carry(i))).asUInt
    io.cout := carry(32)
}

class BLevelPAdder64 extends Module{
    val io = IO(new AdderIO(64))
    val bitPropagate = Wire(Vec(64, Bool()))
    val bitGenerate = Wire(Vec(64, Bool()))
    for (i <- 0 until 64) {
        bitPropagate(i) := io.src1(i) ^ io.src2(i)
        bitGenerate(i) := io.src1(i) & io.src2(i)
    }

    var levelPropagate: Vec[Bool] = bitPropagate
    var levelGenerate: Vec[Bool] = bitGenerate
    var distance = 1
    while (distance < 64) {
        val nextPropagate = Wire(Vec(64, Bool()))
        val nextGenerate = Wire(Vec(64, Bool()))
        for (i <- 0 until 64) {
            if (i >= distance) {
                nextGenerate(i) := levelGenerate(i) |
                    (levelPropagate(i) & levelGenerate(i - distance))
                nextPropagate(i) := levelPropagate(i) & levelPropagate(i - distance)
            } else {
                nextGenerate(i) := levelGenerate(i)
                nextPropagate(i) := levelPropagate(i)
            }
        }
        levelPropagate = nextPropagate
        levelGenerate = nextGenerate
        distance = distance << 1
    }

    val carry = Wire(Vec(65, Bool()))
    carry(0) := io.cin(0)
    for (i <- 0 until 64) {
        carry(i + 1) := levelGenerate(i) | (levelPropagate(i) & io.cin(0))
    }

    io.res := VecInit((0 until 64).map(i => bitPropagate(i) ^ carry(i))).asUInt
    io.cout := carry(64)
}

class PipelinedPrefixAdder64(val preLevels: Int = 2) extends Module {
    require(preLevels >= 1 && preLevels < 6)

    val io = IO(new AdderIO(64) {
        val enable = Input(Bool())
    })

    val basePropagate = Wire(Vec(64, Bool()))
    val baseGenerate = Wire(Vec(64, Bool()))
    for (i <- 0 until 64) {
        basePropagate(i) := io.src1(i) ^ io.src2(i)
        baseGenerate(i) := io.src1(i) & io.src2(i)
    }

    var prePropagate: Vec[Bool] = basePropagate
    var preGenerate: Vec[Bool] = baseGenerate
    var preDistance = 1
    for (_ <- 0 until preLevels) {
        val nextPropagate = Wire(Vec(64, Bool()))
        val nextGenerate = Wire(Vec(64, Bool()))
        for (i <- 0 until 64) {
            if (i >= preDistance) {
                nextGenerate(i) := preGenerate(i) |
                    (prePropagate(i) & preGenerate(i - preDistance))
                nextPropagate(i) := prePropagate(i) & prePropagate(i - preDistance)
            } else {
                nextGenerate(i) := preGenerate(i)
                nextPropagate(i) := prePropagate(i)
            }
        }
        prePropagate = nextPropagate
        preGenerate = nextGenerate
        preDistance = preDistance << 1
    }

    val basePropagateReg = RegInit(VecInit(Seq.fill(64)(false.B)))
    val prePropagateReg = RegInit(VecInit(Seq.fill(64)(false.B)))
    val preGenerateReg = RegInit(VecInit(Seq.fill(64)(false.B)))
    val cinReg = RegInit(false.B)
    when(io.enable) {
        basePropagateReg := basePropagate
        prePropagateReg := prePropagate
        preGenerateReg := preGenerate
        cinReg := io.cin.asBool
    }

    var postPropagate: Vec[Bool] = prePropagateReg
    var postGenerate: Vec[Bool] = preGenerateReg
    var postDistance = 1 << preLevels
    while (postDistance < 64) {
        val nextPropagate = Wire(Vec(64, Bool()))
        val nextGenerate = Wire(Vec(64, Bool()))
        for (i <- 0 until 64) {
            if (i >= postDistance) {
                nextGenerate(i) := postGenerate(i) |
                    (postPropagate(i) & postGenerate(i - postDistance))
                nextPropagate(i) := postPropagate(i) & postPropagate(i - postDistance)
            } else {
                nextGenerate(i) := postGenerate(i)
                nextPropagate(i) := postPropagate(i)
            }
        }
        postPropagate = nextPropagate
        postGenerate = nextGenerate
        postDistance = postDistance << 1
    }

    val carry = Wire(Vec(65, Bool()))
    carry(0) := cinReg
    for (i <- 0 until 64) {
        carry(i + 1) := postGenerate(i) | (postPropagate(i) & cinReg)
    }

    io.res := VecInit((0 until 64).map(i => basePropagateReg(i) ^ carry(i))).asUInt
    io.cout := carry(64)
}

class BLevelPAdder33 extends Module{
    val io = IO(new AdderIO(33))
    val adder32 = BLevelPAdder32(io.src1(31, 0), io.src2(31, 0), io.cin)
    io.res := (adder32.io.cout ^ io.src1(32) ^ io.src2(32)) ## adder32.io.res
    io.cout := (adder32.io.cout & io.src1(32)) | (adder32.io.cout & io.src2(32)) | (io.src1(32) & io.src2(32))
}

object BLevelAdder4 {
    def apply(src1: UInt, src2: UInt, cin: UInt): BLevelAdder4 = {
        val adder = Module(new BLevelAdder4)
        adder.io.src1 := src1
        adder.io.src2 := src2
        adder.io.cin := cin
        adder
    }
}

object BLevelAdder5 {
    def apply(src1: UInt, src2: UInt, cin: UInt): BLevelAdder5 = {
        val adder = Module(new BLevelAdder5)
        adder.io.src1 := src1
        adder.io.src2 := src2
        adder.io.cin := cin
        adder
    }
}

object BLevelPAdder32{
    def apply(src1: UInt, src2: UInt, cin: UInt): BLevelPAdder32 = {
        val adder = Module(new BLevelPAdder32)
        adder.io.src1 := src1
        adder.io.src2 := src2
        adder.io.cin := cin
        adder
    }
}

object BLevelPAdder33{
    def apply(src1: UInt, src2: UInt, cin: UInt): BLevelPAdder33 = {
        val adder = Module(new BLevelPAdder33)
        adder.io.src1 := src1
        adder.io.src2 := src2
        adder.io.cin := cin
        adder
    }
}
object BLevelPAdder64{
    def apply(src1: UInt, src2: UInt, cin: UInt): BLevelPAdder64 = {
        val adder = Module(new BLevelPAdder64)
        adder.io.src1 := src1
        adder.io.src2 := src2
        adder.io.cin := cin
        adder
    }
}
