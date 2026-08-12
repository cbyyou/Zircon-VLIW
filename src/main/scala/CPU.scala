import chisel3._
import chisel3.util._

// CPU调试接口
class CPUDebugIO extends Bundle {
    // 寄存器堆
    val gpr = Output(Vec(32, UInt(32.W)))  // GPR寄存器堆
    val fpr = Output(Vec(32, UInt(32.W)))  // FPR寄存器堆
    
    // 8条流水线的WB阶段提交信息
    val wbValid = Output(Vec(8, Bool()))       // 是否有效提交
    val wbPC = Output(Vec(8, UInt(32.W)))      // 提交的PC
    val wbInst = Output(Vec(8, UInt(32.W)))    // 提交的指令
    val wbRd = Output(Vec(8, UInt(6.W)))       // 写回的寄存器（6位，最高位区分GPR/FPR）
    val wbData = Output(Vec(8, UInt(32.W)))    // 写回的数据
    
    // 分支调试信号
    val predFail = Output(Bool())
    val branchTgt = Output(UInt(32.W))
    val hazardFlush = Output(Bool())
    val hazardStall = Output(Bool())
}

// CPU的顶层IO：对外提供仿真环境的内存接口
class CPUIO extends Bundle {
    val imem = new FrontendMemIO     // 取指内存接口
    val dmem = new BackendMemIO      // 数据内存接口（两个LSU）
    val debug = new CPUDebugIO       // 调试接口
}

class CPU extends Module {
    val io = IO(new CPUIO)
    
    // ========== 实例化三大模块 ==========
    val frontend = Module(new Frontend)
    val backend = Module(new Backend)
    val hazard = Module(new Hazard)
    
    // ========== 连接Frontend和Backend ==========
    // Frontend -> Backend: 指令包
    backend.io.frontend.instPkgs := frontend.io.backend.instPkg
    
    // Backend -> Frontend: 写回信号
    frontend.io.backend.gprWen := backend.io.frontend.gprWen
    frontend.io.backend.gprWaddr := backend.io.frontend.gprWaddr
    frontend.io.backend.gprWdata := backend.io.frontend.gprWdata
    frontend.io.backend.fprWen := backend.io.frontend.fprWen
    frontend.io.backend.fprWaddr := backend.io.frontend.fprWaddr
    frontend.io.backend.fprWdata := backend.io.frontend.fprWdata
    
    // Backend -> Frontend: 分支重定向
    frontend.io.backend.predFail := backend.io.frontend.predFail
    frontend.io.backend.branchTgt := backend.io.frontend.branchTgt
    frontend.io.backend.branchUpdateValid := backend.io.frontend.branchUpdateValid
    frontend.io.backend.branchUpdatePC := backend.io.frontend.branchUpdatePC
    frontend.io.backend.branchUpdateInst := backend.io.frontend.branchUpdateInst
    frontend.io.backend.branchUpdateTaken := backend.io.frontend.branchUpdateTaken
    frontend.io.backend.branchUpdateTarget := backend.io.frontend.branchUpdateTarget
    
    // ========== 连接Frontend和Hazard ==========
    frontend.io.hazard <> hazard.io.frontend
    
    // ========== 连接Backend和Hazard ==========
    backend.io.hazard <> hazard.io.backend
    
    // ========== 连接仿真环境内存接口 ==========
    // 取指接口
    io.imem <> frontend.io.mem
    
    // 数据访存接口（两个LSU）
    io.dmem <> backend.io.mem
    
    // ========== 连接调试接口 ==========
    io.debug.gpr := frontend.io.debug.gpr
    io.debug.fpr := frontend.io.debug.fpr
    io.debug.wbValid := backend.io.debug.wbValid
    io.debug.wbPC := backend.io.debug.wbPC
    io.debug.wbInst := backend.io.debug.wbInst
    io.debug.wbRd := backend.io.debug.wbRd
    io.debug.wbData := backend.io.debug.wbData
    
    // 分支调试信号
    io.debug.predFail := backend.io.frontend.predFail
    io.debug.branchTgt := backend.io.frontend.branchTgt
    io.debug.hazardFlush := hazard.io.frontend.flush
    io.debug.hazardStall := hazard.io.frontend.stall
}
