import chisel3._
import chisel3.util._

class PerformanceMonitorIO extends Bundle {
    val wbValid = Input(Vec(8, Bool()))
    val wbInst = Input(Vec(8, UInt(32.W)))
    val rawStall = Input(Bool())
    val rawLoad = Input(Bool())
    val rawIntegerMul = Input(Bool())
    val rawFloatAdd = Input(Bool())
    val rawFloatMul = Input(Bool())
    val rawFloatDiv = Input(Bool())
    val falseRawStall = Input(Bool())
    val branchRedirect = Input(Bool())
    val executionStall = Input(Bool())
    val floatDivStall = Input(Bool())
    val integerDivStall = Input(Bool())
    val integerDivStarts = Input(UInt(2.W))
    val integerDivStartsOnRedirect = Input(UInt(2.W))

    val cycles = Output(UInt(64.W))
    val effectiveInstructions = Output(UInt(64.W))
    val executedPackets = Output(UInt(64.W))
    val rawStallCycles = Output(UInt(64.W))
    val rawLoadCycles = Output(UInt(64.W))
    val rawIntegerMulCycles = Output(UInt(64.W))
    val rawFloatAddCycles = Output(UInt(64.W))
    val rawFloatMulCycles = Output(UInt(64.W))
    val rawFloatDivCycles = Output(UInt(64.W))
    val falseRawStallCycles = Output(UInt(64.W))
    val rawOtherCycles = Output(UInt(64.W))
    val branchRedirects = Output(UInt(64.W))
    val executionStallCycles = Output(UInt(64.W))
    val floatDivStallCycles = Output(UInt(64.W))
    val integerDivStallCycles = Output(UInt(64.W))
    val integerDivStartEvents = Output(UInt(64.W))
    val integerDivStartOnRedirectEvents = Output(UInt(64.W))
    val conditionalBranches = Output(UInt(64.W))
    val jumps = Output(UInt(64.W))
    val integerMultiplyInstructions = Output(UInt(64.W))
    val integerDivideInstructions = Output(UInt(64.W))
    val floatAddInstructions = Output(UInt(64.W))
    val floatMultiplyInstructions = Output(UInt(64.W))
    val floatDivideInstructions = Output(UInt(64.W))
    val floatMacInstructions = Output(UInt(64.W))
    val loadInstructions = Output(UInt(64.W))
    val storeInstructions = Output(UInt(64.W))
}

class PerformanceMonitor extends Module {
    val io = IO(new PerformanceMonitorIO)

    val nopInstruction = "h00000013".U(32.W)
    val fillerFeqInstruction = "ha0002053".U(32.W)
    val haltInstruction = "h80000000".U(32.W)

    val cycleCount = RegInit(0.U(64.W))
    val effectiveInstructionCount = RegInit(0.U(64.W))
    val executedPacketCount = RegInit(0.U(64.W))
    val rawStallCount = RegInit(0.U(64.W))
    val rawLoadCount = RegInit(0.U(64.W))
    val rawIntegerMulCount = RegInit(0.U(64.W))
    val rawFloatAddCount = RegInit(0.U(64.W))
    val rawFloatMulCount = RegInit(0.U(64.W))
    val rawFloatDivCount = RegInit(0.U(64.W))
    val falseRawStallCount = RegInit(0.U(64.W))
    val rawOtherCount = RegInit(0.U(64.W))
    val branchRedirectCount = RegInit(0.U(64.W))
    val executionStallCount = RegInit(0.U(64.W))
    val floatDivStallCount = RegInit(0.U(64.W))
    val integerDivStallCount = RegInit(0.U(64.W))
    val integerDivStartCount = RegInit(0.U(64.W))
    val integerDivStartOnRedirectCount = RegInit(0.U(64.W))
    val conditionalBranchCount = RegInit(0.U(64.W))
    val jumpCount = RegInit(0.U(64.W))
    val integerMultiplyInstructionCount = RegInit(0.U(64.W))
    val integerDivideInstructionCount = RegInit(0.U(64.W))
    val floatAddInstructionCount = RegInit(0.U(64.W))
    val floatMultiplyInstructionCount = RegInit(0.U(64.W))
    val floatDivideInstructionCount = RegInit(0.U(64.W))
    val floatMacInstructionCount = RegInit(0.U(64.W))
    val loadInstructionCount = RegInit(0.U(64.W))
    val storeInstructionCount = RegInit(0.U(64.W))

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
    val conditionalBranchCommits = VecInit((0 until 8).map { i =>
        effectiveCommits(i) && io.wbInst(i)(6, 0) === "h63".U
    })
    val jumpCommits = VecInit((0 until 8).map { i =>
        val opcode = io.wbInst(i)(6, 0)
        effectiveCommits(i) && (opcode === "h6f".U || opcode === "h67".U)
    })
    val integerMultiplyCommits = VecInit((0 until 8).map { i =>
        effectiveCommits(i) && io.wbInst(i)(6, 0) === "h33".U &&
        io.wbInst(i)(31, 25) === 1.U && !io.wbInst(i)(14)
    })
    val integerDivideCommits = VecInit((0 until 8).map { i =>
        effectiveCommits(i) && io.wbInst(i)(6, 0) === "h33".U &&
        io.wbInst(i)(31, 25) === 1.U && io.wbInst(i)(14)
    })
    val floatAddCommits = VecInit((0 until 8).map { i =>
        val funct7 = io.wbInst(i)(31, 25)
        effectiveCommits(i) && io.wbInst(i)(6, 0) === "h53".U &&
        (funct7 === "h00".U || funct7 === "h04".U)
    })
    val floatMultiplyCommits = VecInit((0 until 8).map { i =>
        effectiveCommits(i) && io.wbInst(i)(6, 0) === "h53".U &&
        io.wbInst(i)(31, 25) === "h08".U
    })
    val floatDivideCommits = VecInit((0 until 8).map { i =>
        val funct7 = io.wbInst(i)(31, 25)
        effectiveCommits(i) && io.wbInst(i)(6, 0) === "h53".U &&
        (funct7 === "h0c".U || funct7 === "h2c".U)
    })
    val floatMacCommits = VecInit((0 until 8).map { i =>
        val opcode = io.wbInst(i)(6, 0)
        effectiveCommits(i) && Seq("h43".U, "h47".U, "h4b".U, "h4f".U)
            .map(opcode === _).reduce(_ || _)
    })
    val loadCommits = VecInit((0 until 8).map { i =>
        val opcode = io.wbInst(i)(6, 0)
        effectiveCommits(i) && (opcode === "h03".U || opcode === "h07".U)
    })
    val storeCommits = VecInit((0 until 8).map { i =>
        val opcode = io.wbInst(i)(6, 0)
        effectiveCommits(i) && (opcode === "h23".U || opcode === "h27".U)
    })
    val rawOther = io.rawStall && !(
        io.rawLoad || io.rawIntegerMul || io.rawFloatAdd || io.rawFloatMul || io.rawFloatDiv
    )

    cycleCount := cycleCount + 1.U
    effectiveInstructionCount := effectiveInstructionCount + PopCount(effectiveCommits)
    when(packetCommitted && !packetContainsHalt) {
        executedPacketCount := executedPacketCount + 1.U
    }
    rawStallCount := rawStallCount + io.rawStall
    rawLoadCount := rawLoadCount + io.rawLoad
    rawIntegerMulCount := rawIntegerMulCount + io.rawIntegerMul
    rawFloatAddCount := rawFloatAddCount + io.rawFloatAdd
    rawFloatMulCount := rawFloatMulCount + io.rawFloatMul
    rawFloatDivCount := rawFloatDivCount + io.rawFloatDiv
    falseRawStallCount := falseRawStallCount + io.falseRawStall
    rawOtherCount := rawOtherCount + rawOther
    branchRedirectCount := branchRedirectCount + io.branchRedirect
    executionStallCount := executionStallCount + io.executionStall
    floatDivStallCount := floatDivStallCount + io.floatDivStall
    integerDivStallCount := integerDivStallCount + io.integerDivStall
    integerDivStartCount := integerDivStartCount + io.integerDivStarts
    integerDivStartOnRedirectCount := integerDivStartOnRedirectCount + io.integerDivStartsOnRedirect
    conditionalBranchCount := conditionalBranchCount + PopCount(conditionalBranchCommits)
    jumpCount := jumpCount + PopCount(jumpCommits)
    integerMultiplyInstructionCount := integerMultiplyInstructionCount + PopCount(integerMultiplyCommits)
    integerDivideInstructionCount := integerDivideInstructionCount + PopCount(integerDivideCommits)
    floatAddInstructionCount := floatAddInstructionCount + PopCount(floatAddCommits)
    floatMultiplyInstructionCount := floatMultiplyInstructionCount + PopCount(floatMultiplyCommits)
    floatDivideInstructionCount := floatDivideInstructionCount + PopCount(floatDivideCommits)
    floatMacInstructionCount := floatMacInstructionCount + PopCount(floatMacCommits)
    loadInstructionCount := loadInstructionCount + PopCount(loadCommits)
    storeInstructionCount := storeInstructionCount + PopCount(storeCommits)

    io.cycles := cycleCount
    io.effectiveInstructions := effectiveInstructionCount
    io.executedPackets := executedPacketCount
    io.rawStallCycles := rawStallCount
    io.rawLoadCycles := rawLoadCount
    io.rawIntegerMulCycles := rawIntegerMulCount
    io.rawFloatAddCycles := rawFloatAddCount
    io.rawFloatMulCycles := rawFloatMulCount
    io.rawFloatDivCycles := rawFloatDivCount
    io.falseRawStallCycles := falseRawStallCount
    io.rawOtherCycles := rawOtherCount
    io.branchRedirects := branchRedirectCount
    io.executionStallCycles := executionStallCount
    io.floatDivStallCycles := floatDivStallCount
    io.integerDivStallCycles := integerDivStallCount
    io.integerDivStartEvents := integerDivStartCount
    io.integerDivStartOnRedirectEvents := integerDivStartOnRedirectCount
    io.conditionalBranches := conditionalBranchCount
    io.jumps := jumpCount
    io.integerMultiplyInstructions := integerMultiplyInstructionCount
    io.integerDivideInstructions := integerDivideInstructionCount
    io.floatAddInstructions := floatAddInstructionCount
    io.floatMultiplyInstructions := floatMultiplyInstructionCount
    io.floatDivideInstructions := floatDivideInstructionCount
    io.floatMacInstructions := floatMacInstructionCount
    io.loadInstructions := loadInstructionCount
    io.storeInstructions := storeInstructionCount
}
