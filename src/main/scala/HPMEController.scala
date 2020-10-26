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

case class HPMEControllerParams(address: BigInt, beatBytes: Int)

trait HPMEControllerBundle extends Bundle{
  // DMA Ctrl Signals
  val dmaReq = Decoupled(UInt(DMA_REQ_WIDTH.W))
  val dmaComplete = Input(Bool())
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
  val dmaReqReg = RegInit(0.U(DMA_REQ_WIDTH.W))
  val dmaCompleteReg = RegInit(false.B)

  io.dmaReq.valid := (dmaReqReg =/= 0.U)
  io.dmaReq.bits := dmaReqReg
  dmaCompleteReg := (io.dmaComplete) && (dmaReqReg =/= 0.U)
  printf("[HPME Log] dmaReqReg = %x, dmaCompleteReg = %d\n", dmaReqReg, dmaCompleteReg)
  regmap(
    0x00 -> Seq(RegField.w(DMA_REQ_WIDTH, dmaReqReg)), // when written a number, valid will be set
    0x08 -> Seq(RegField.r(1, dmaCompleteReg))
  )
}

class HPMEController(c: HPMEControllerParams)(implicit p: Parameters) extends TLRegisterRouter(
  c.address, "hpmecontroller", Seq("hpme,hpmecontroller"), beatBytes = c.beatBytes)(
  new TLRegBundle(c,_) with HPMEControllerBundle)(
  new TLRegModule(c,_,_) with HPMEContrllerModule)