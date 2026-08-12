import chisel3._
import chiseltest._
import org.scalatest.flatspec.AnyFlatSpec
import ZirconConfig.EXEOp._

class HazardStallCounterTest extends AnyFlatSpec with ChiselScalatestTester {
  behavior of "StallPerformanceMonitor"

  private def clearEvents(c: StallPerformanceMonitor): Unit = {
    c.io.events.rawStall.poke(false.B)
    c.io.events.loadRaw.poke(false.B)
    c.io.events.integerRaw.poke(false.B)
    c.io.events.floatRaw.poke(false.B)
    c.io.events.intDivBusy.poke(false.B)
    c.io.events.fdivBusy.poke(false.B)
    c.io.events.fsqrtBusy.poke(false.B)
    c.io.events.branchResolved.poke(false.B)
    c.io.events.branchTaken.poke(false.B)
    c.io.events.branchMispredict.poke(false.B)
    c.io.events.branchFlush.poke(false.B)
  }

  private def stepEvent(c: StallPerformanceMonitor)(drive: => Unit): Unit = {
    clearEvents(c)
    drive
    c.clock.step()
  }

  private def clearPackage(pkg: InstructionPackage): Unit = {
    pkg.pc.poke(0.U)
    pkg.inst.poke(0.U)
    pkg.predTaken.poke(false.B)
    pkg.predTarget.poke(0.U)
    pkg.rs1.poke(0.U)
    pkg.rs2.poke(0.U)
    pkg.rs3.poke(0.U)
    pkg.rd.poke(0.U)
    pkg.rdValid.poke(false.B)
    pkg.rs1Data.poke(0.U)
    pkg.rs2Data.poke(0.U)
    pkg.rs3Data.poke(0.U)
    pkg.op.poke(0.U)
    pkg.rm.poke(0.U)
    pkg.imm.poke(0.U)
    pkg.src1Sel.poke(0.U)
    pkg.src2Sel.poke(0.U)
    pkg.aluResult.poke(0.U)
    pkg.fpuResult.poke(0.U)
    pkg.fflags.poke(0.U)
    pkg.branchTgt.poke(0.U)
    pkg.predFail.poke(false.B)
    pkg.branchTaken.poke(false.B)
    pkg.actualBranchTarget.poke(0.U)
    pkg.memResult.poke(0.U)
    pkg.rfWdata.poke(0.U)
  }

  private def clearHazard(c: Hazard): Unit = {
    for (i <- 0 until 8) {
      clearPackage(c.io.frontend.idPkgs(i))
      clearPackage(c.io.backend.ex1Pkgs(i))
      clearPackage(c.io.backend.ex2Pkgs(i))
      clearPackage(c.io.backend.ex3Pkgs(i))
    }
    c.io.backend.divBusy(0).poke(false.B)
    c.io.backend.divBusy(1).poke(false.B)
    c.io.backend.fdivBusy.poke(false.B)
    c.io.backend.predFail.poke(false.B)
    c.io.backend.branchTgt.poke(0.U)
  }

  it should "count mutually exclusive RAW and long-latency causes" in {
    test(new StallPerformanceMonitor).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearEvents(c)
      c.clock.step(2)

      stepEvent(c) {
        c.io.events.rawStall.poke(true.B)
        c.io.events.loadRaw.poke(true.B)
      }
      stepEvent(c) {
        c.io.events.rawStall.poke(true.B)
        c.io.events.integerRaw.poke(true.B)
      }
      stepEvent(c) {
        c.io.events.rawStall.poke(true.B)
        c.io.events.floatRaw.poke(true.B)
      }
      stepEvent(c) {
        c.io.events.rawStall.poke(true.B)
        c.io.events.loadRaw.poke(true.B)
        c.io.events.floatRaw.poke(true.B)
      }

      stepEvent(c) {
        c.io.events.intDivBusy.poke(true.B)
      }
      stepEvent(c) {
        c.io.events.fdivBusy.poke(true.B)
      }
      stepEvent(c) {
        c.io.events.fsqrtBusy.poke(true.B)
      }
      stepEvent(c) {
        c.io.events.intDivBusy.poke(true.B)
        c.io.events.fdivBusy.poke(true.B)
      }

      stepEvent(c) {
        c.io.events.branchResolved.poke(true.B)
        c.io.events.branchTaken.poke(true.B)
        c.io.events.branchMispredict.poke(true.B)
        c.io.events.branchFlush.poke(true.B)
      }

      c.io.counters.cycleCount.expect(11.U)
      c.io.counters.totalStallCycles.expect(8.U)
      c.io.counters.rawStallCycles.expect(4.U)
      c.io.counters.loadRawStallCycles.expect(1.U)
      c.io.counters.integerRawStallCycles.expect(1.U)
      c.io.counters.floatRawStallCycles.expect(1.U)
      c.io.counters.mixedRawStallCycles.expect(1.U)
      c.io.counters.intDivOnlyStallCycles.expect(1.U)
      c.io.counters.fdivOnlyStallCycles.expect(1.U)
      c.io.counters.fsqrtOnlyStallCycles.expect(1.U)
      c.io.counters.longLatencyOverlapStallCycles.expect(1.U)
      c.io.counters.branchCount.expect(1.U)
      c.io.counters.branchTakenCount.expect(1.U)
      c.io.counters.branchMispredictCount.expect(1.U)
      c.io.counters.branchFlushCycles.expect(1.U)
    }
  }

  it should "classify accepted Hazard decisions without affecting control" in {
    test(new Hazard(enablePerfCounters = true)).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearHazard(c)
      c.io.frontend.idPkgs(0).rs1.poke(1.U)

      c.io.backend.ex1Pkgs(5).rd.poke(1.U)
      c.io.backend.ex1Pkgs(5).rdValid.poke(true.B)
      c.io.backend.ex1Pkgs(5).op.poke(LW)
      c.io.events.rawStall.expect(true.B)
      c.io.events.loadRaw.expect(true.B)
      c.io.events.integerRaw.expect(false.B)
      c.io.events.floatRaw.expect(false.B)

      clearPackage(c.io.backend.ex1Pkgs(5))
      c.io.backend.ex1Pkgs(3).rd.poke(1.U)
      c.io.backend.ex1Pkgs(3).rdValid.poke(true.B)
      c.io.backend.ex1Pkgs(3).op.poke(MUL)
      c.io.events.loadRaw.expect(false.B)
      c.io.events.integerRaw.expect(true.B)
      c.io.events.floatRaw.expect(false.B)

      clearPackage(c.io.backend.ex1Pkgs(3))
      c.io.backend.ex1Pkgs(1).rd.poke(1.U)
      c.io.backend.ex1Pkgs(1).rdValid.poke(true.B)
      c.io.backend.ex1Pkgs(1).op.poke(FADD_S)
      c.io.events.loadRaw.expect(false.B)
      c.io.events.integerRaw.expect(false.B)
      c.io.events.floatRaw.expect(true.B)

      c.io.backend.ex1Pkgs(5).rd.poke(1.U)
      c.io.backend.ex1Pkgs(5).rdValid.poke(true.B)
      c.io.backend.ex1Pkgs(5).op.poke(LW)
      c.io.events.loadRaw.expect(true.B)
      c.io.events.floatRaw.expect(true.B)

      c.io.backend.divBusy(0).poke(true.B)
      c.io.events.rawStall.expect(false.B)
      c.io.events.intDivBusy.expect(true.B)

      c.io.backend.divBusy(0).poke(false.B)
      c.io.backend.fdivBusy.poke(true.B)
      c.io.backend.ex2Pkgs(0).op.poke(FDIV_S)
      c.io.events.fdivBusy.expect(true.B)
      c.io.events.fsqrtBusy.expect(false.B)

      c.io.backend.ex2Pkgs(0).op.poke(FSQRT_S)
      c.io.events.fdivBusy.expect(false.B)
      c.io.events.fsqrtBusy.expect(true.B)

      c.io.backend.fdivBusy.poke(false.B)
      c.io.backend.ex2Pkgs(7).op.poke(BEQ)
      c.io.backend.ex2Pkgs(7).branchTaken.poke(true.B)
      c.io.backend.predFail.poke(true.B)
      c.io.events.branchResolved.expect(true.B)
      c.io.events.branchTaken.expect(true.B)
      c.io.events.branchMispredict.expect(true.B)
      c.io.events.branchFlush.expect(true.B)
    }
  }

  it should "release integer multiply consumers when the producer reaches EX3" in {
    test(new Hazard).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearHazard(c)
      c.io.frontend.idPkgs(0).rs1.poke(1.U)

      c.io.backend.ex1Pkgs(3).rd.poke(1.U)
      c.io.backend.ex1Pkgs(3).rdValid.poke(true.B)
      c.io.backend.ex1Pkgs(3).op.poke(MUL)
      c.io.frontend.stall.expect(true.B)

      clearPackage(c.io.backend.ex1Pkgs(3))
      c.io.backend.ex2Pkgs(3).rd.poke(1.U)
      c.io.backend.ex2Pkgs(3).rdValid.poke(true.B)
      c.io.backend.ex2Pkgs(3).op.poke(MUL)
      c.io.frontend.stall.expect(false.B)

      clearPackage(c.io.backend.ex2Pkgs(3))
      Seq(MUL, MULH, MULHU, MULHSU).foreach { op =>
        c.io.backend.ex3Pkgs(3).rd.poke(1.U)
        c.io.backend.ex3Pkgs(3).rdValid.poke(true.B)
        c.io.backend.ex3Pkgs(3).op.poke(op)
        c.io.frontend.stall.expect(false.B)
      }

      c.io.backend.ex3Pkgs(3).op.poke(DIV)
      c.io.frontend.stall.expect(true.B)
    }
  }

  it should "hold only true EX2 multiply dependencies feeding control instructions" in {
    test(new Hazard).withAnnotations(Seq(TreadleBackendAnnotation)) { c =>
      clearHazard(c)

      val conditionalBranches = Seq(BEQ, BNE, BLT, BGE, BLTU, BGEU)
      val multiplyOps = Seq(MUL, MULH, MULHU, MULHSU)

      for {
        producerLane <- Seq(3, 4)
        multiplyOp <- multiplyOps
        branchOp <- conditionalBranches
        source <- Seq(1, 2)
      } {
        clearHazard(c)
        c.io.frontend.idPkgs(7).op.poke(branchOp)
        c.io.frontend.idPkgs(7).rs1.poke(11.U)
        c.io.frontend.idPkgs(7).rs2.poke(12.U)
        c.io.backend.ex2Pkgs(producerLane).rd.poke((10 + source).U)
        c.io.backend.ex2Pkgs(producerLane).rdValid.poke(true.B)
        c.io.backend.ex2Pkgs(producerLane).op.poke(multiplyOp)
        c.io.frontend.stall.expect(true.B)
      }

      clearHazard(c)
      c.io.frontend.idPkgs(7).op.poke(JALR)
      c.io.frontend.idPkgs(7).rs1.poke(11.U)
      c.io.frontend.idPkgs(7).rs2.poke(12.U)
      c.io.backend.ex2Pkgs(3).rd.poke(11.U)
      c.io.backend.ex2Pkgs(3).rdValid.poke(true.B)
      c.io.backend.ex2Pkgs(3).op.poke(MUL)
      c.io.frontend.stall.expect(true.B)

      clearHazard(c)
      c.io.frontend.idPkgs(7).op.poke(JALR)
      c.io.frontend.idPkgs(7).rs1.poke(11.U)
      c.io.frontend.idPkgs(7).rs2.poke(12.U)
      c.io.backend.ex2Pkgs(3).rd.poke(12.U)
      c.io.backend.ex2Pkgs(3).rdValid.poke(true.B)
      c.io.backend.ex2Pkgs(3).op.poke(MUL)
      c.io.frontend.stall.expect(false.B)

      for (nonConsumerOp <- Seq(JAL, ADD)) {
        clearHazard(c)
        c.io.frontend.idPkgs(7).op.poke(nonConsumerOp)
        c.io.frontend.idPkgs(7).rs1.poke(11.U)
        c.io.frontend.idPkgs(7).rs2.poke(11.U)
        c.io.backend.ex2Pkgs(3).rd.poke(11.U)
        c.io.backend.ex2Pkgs(3).rdValid.poke(true.B)
        c.io.backend.ex2Pkgs(3).op.poke(MUL)
        c.io.frontend.stall.expect(false.B)
      }
    }
  }
}
