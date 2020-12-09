package hpme

import chisel3._
import chisel3.util._
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes}
import freechips.rocketchip.config.{Parameters, Field}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf
import testchipip.TLHelper

import HPMEConsts._

/*
 * HPMEContrller(DMA is embedded in) is on PeripheryBus, DMA Read Node and DMA Write Node are on FrontBus.
 */

case class HPMEControllerParams(address: BigInt,
                                beatBytes: Int,
                                devName: String)

trait HPMEControllerBundle extends Bundle{
  // DMA Ctrl Signals
  val dmaReq = Decoupled(UInt(DMA_REQ_WIDTH.W))
  val dmaComplete = Input(Bool())
  val hpmeIsIdle = Input(Bool())
  val key = Decoupled(UInt(ENC_WIDTH.W))
  val counter = Decoupled(UInt(ENC_WIDTH.W))
  val mac = Flipped(Decoupled(UInt(ENC_WIDTH.W)))
  val isEnc = Output(Bool())
}

/*
 * When dmaReqReg =/= 0.U, CPU has sent DMA request to HPME
 * when io.dmaComplete === true.B, DMA request has been completed,
 * and dmaCompleteReg will be set to true.B. CPU will set dmaReqReg to 0.U
 * to end this request after reading dmaCompleteReg as 0.B
 *
 * BaseAddr + 0x00 maps to dmaReqReg
 * BaseAddr + 0x08 maps to dmaCompleteReg
 */

trait HPMEContrllerModule extends HasRegMap{
  val io: HPMEControllerBundle
  implicit val p: Parameters
  def params: HPMEControllerParams
  // MMIOs
  val dmaReqReg = RegInit(0.U(DMA_REQ_WIDTH.W)) // (62,61): 0 data, 1 key, 2 MAC (60,50): transfer words,
                                                // (49,2): base addr, (1,0): 2 write 1 read
  val dmaCompleteReg = RegInit(false.B)
  val dataInLo = Reg(UInt(XLEN.W))
  val dataInHi = Reg(UInt(XLEN.W))
  val dataInType = RegInit(0.U(XLEN.W))
  val dataInRcvd = RegInit(false.B)
  val isEnc = RegInit(false.B)
  val dataOutLo = Reg(UInt(XLEN.W))
  val dataOutHi = Reg(UInt(XLEN.W))
  val dataOutType = RegInit(0.U(XLEN.W))
  val dataOutRcvd = RegInit(false.B)

  io.dmaReq.valid := (dmaReqReg =/= 0.U)
  io.dmaReq.bits := dmaReqReg
  dmaCompleteReg := (io.dmaComplete) && (dmaReqReg =/= 0.U)

  val s_idle :: s_trans :: s_done :: Nil = Enum(3)
  val tranState = RegInit(s_idle)

  val macs_idle :: macs_sent :: macs_rcvd :: Nil = Enum(3)
  val macState = RegInit(macs_idle)

  dataInRcvd := tranState === s_done
  io.key.valid := dataInType === DMA_REQ_DATA_TYPE_KEY && tranState === s_trans
  io.counter.valid := dataInType === DMA_REQ_DATA_TYPE_CNT && tranState === s_trans
  io.key.bits := Cat(dataInHi, dataInLo)
  io.counter.bits := Cat(dataInHi, dataInLo)
  io.isEnc := isEnc
  io.mac.ready := macState === macs_idle

  switch(tranState){
    is(s_idle){
      when(dataInType =/= 0.U){
        tranState := s_trans
      }
    }
    is(s_trans){
      when(io.key.fire() || io.counter.fire()){
        tranState := s_done
      }
    }
    is(s_done){
      when(dataInType === 0.U){
        tranState := s_idle
      }
    }
  }

  when(io.mac.fire()){
    dataOutLo := io.mac.bits(63,0)
    dataOutHi := io.mac.bits(127,64)
  }
  dataOutType := Mux(macState === macs_sent, DATA_OUT_TYPE_MAC, DATA_OUT_TYPE_NONE)
  switch(macState){
    is(macs_idle){
      when(io.mac.fire()){
        macState := macs_sent
      }
    }
    is(macs_sent){
      when(dataOutRcvd){
        macState := macs_rcvd
      }
    }
    is(macs_rcvd){
      when(!dataOutRcvd){
        macState := macs_idle
      }
    }
  }

  printf("[HPME Log] dmaReqReg = %x, dmaCompleteReg = %d\n", dmaReqReg, dmaCompleteReg)
  regmap(
    0x00 -> Seq(RegField.w(DMA_REQ_WIDTH, dmaReqReg)), // when written a number, valid will be set
    0x08 -> Seq(RegField.r(1, dmaCompleteReg)),
    0x10 -> Seq(RegField.w(XLEN, dataInLo)),
    0x18 -> Seq(RegField.w(XLEN, dataInHi)),
    0x20 -> Seq(RegField.w(XLEN, dataInType)),
    0x28 -> Seq(RegField.r(1, dataInRcvd)),
    0x29 -> Seq(RegField.w(1, isEnc)),
    0x30 -> Seq(RegField.r(XLEN, dataOutLo)),
    0x38 -> Seq(RegField.r(XLEN, dataOutHi)),
    0x40 -> Seq(RegField.r(XLEN, dataOutType)),
    0x48 -> Seq(RegField.w(1, dataOutRcvd))
  )
}

class HPMEController(c: HPMEControllerParams)(implicit p: Parameters) extends TLRegisterRouter(
  c.address, c.devName, Seq("hpme,hpmecontroller"), beatBytes = c.beatBytes)(
  new TLRegBundle(c,_) with HPMEControllerBundle)(
  new TLRegModule(c,_,_) with HPMEContrllerModule)