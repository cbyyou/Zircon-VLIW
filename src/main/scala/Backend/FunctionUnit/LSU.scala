import chisel3._
import chisel3.util._
import ZirconConfig.EXEOp._
import ZirconUtil._

// LSU内存接口
class LSUMemIO extends Bundle {
    val valid = Output(Bool())
    val op    = Output(UInt(7.W))
    val addr  = Output(UInt(32.W))
    val wdata = Output(UInt(32.W))
    val rdata = Input(UInt(32.W))
}

class LSUIO extends Bundle {
    val valid = Input(Bool())
    val op    = Input(UInt(7.W))
    val addr  = Input(UInt(32.W))
    val wdata = Input(UInt(32.W))
    val mem   = new LSUMemIO
    val res   = Output(UInt(32.W))
}

class LSU extends Module {
    val io = IO(new LSUIO)
    
    // 将操作、地址、写数据直接送给仿真环境
    io.mem.op    := io.op
    io.mem.addr  := io.addr
    io.mem.wdata := io.wdata
    io.mem.valid := io.valid
    // 从仿真环境读取的数据
    val memData = io.mem.rdata
    
    // 根据load指令类型进行扩展
    val res = WireDefault(memData)
    switch(io.op) {
        is(LB) {
            // 符号扩展8位
            res := SE(memData(7, 0), 32)
        }
        is(LH) {
            // 符号扩展16位
            res := SE(memData(15, 0), 32)
        }
        is(LW) {
            // 32位直接返回
            res := memData
        }
        is(LBU) {
            // 零扩展8位
            res := ZE(memData(7, 0), 32)
        }
        is(LHU) {
            // 零扩展16位
            res := ZE(memData(15, 0), 32)
        }
        is(FLW) {
            // 浮点加载32位
            res := memData
        }
    }
    
    io.res := res
}
