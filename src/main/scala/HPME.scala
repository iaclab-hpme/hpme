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

case class HPMEConfig()

case object HPMEKey extends Field[Option[HPMEConfig]](None)

class HPME(c: HPMEControllerParams)(implicit p:Parameters) extends LazyModule {
  val controller = LazyModule(new HPMEController(c))
  val dmaReader = LazyModule(new DMAReader)
  val dmaWriter = LazyModule(new DMAWriter)

  val hpmeNode = TLIdentityNode()
  hpmeNode := TLWidthWidget(8) := dmaReader.node // bus with 8-bytes width
  hpmeNode := TLWidthWidget(8) := dmaWriter.node

  lazy val module = new LazyModuleImp(this) {
    val ren = RegInit(false.B) // read enable
    val wen = RegInit(false.B) // write enable
    val dmaReqType = RegInit(0.U(2.W)) // 1.U: read req, 2.U: write req

    /*
     * create and init data buffer and helper vars
     */
    val bufferSize = BUFFER_SIZE // buffer is 512 * 64bit
    val bufferSizeBits = log2Ceil(bufferSize)
    val initSeq = for(i <- 1 until (bufferSize + 1)) yield i.U(BUFFER_WIDTH.W)
    val regBuffer = RegInit(VecInit(initSeq))
    val idxR = RegInit(0.U(bufferSizeBits.W))
    val idxW = RegInit(0.U(bufferSizeBits.W))

    /*
     * Decode DMA request
     */
    dmaReqType := controller.module.io.dmaReq.bits(1,0)
    val dmaReqAddress = Cat(0.U(16), controller.module.io.dmaReq.bits(49,2))
    val dmaReqBytesSize = controller.module.io.dmaReq.bits(62, 50)
    val dmaReqValid = controller.module.io.dmaReq.valid

    /*
     * Read data to buffer
     * dmaReader.module.io.data.valid will be set for one cycle when DMAReader sends data to buffer
     * dmaReader.module.io.data.ready will be set for one cycle after buffer is written
     */
    dmaReader.module.io.req.bits.address := dmaReqAddress
    dmaReader.module.io.req.bits.bytesSize := dmaReqBytesSize
    dmaReader.module.io.req.valid := dmaReqValid && (dmaReqType === DMA_REQ_TYPE_READ)

    // Read state machine
    ren := MuxCase(ren, Seq( // Keep current state
      ((dmaReader.module.io.req.ready && (dmaReqType === DMA_REQ_TYPE_READ)) -> true.B), // read enable
      (dmaReader.module.io.complete.valid -> false.B) // read disable
    ))

    dmaReader.module.io.data.ready := false.B
    when(ren && dmaReader.module.io.data.valid){ // valid will be high for one cycle when data comes
      regBuffer(idxR) := dmaReader.module.io.data.bits
      dmaReader.module.io.data.ready := true.B
      idxR := Mux(idxR === (bufferSize-1).U, 0.U, idxR + 1.U)
      printf("[HPME Log] idxR = %d, read %d from memory\n", idxR, dmaReader.module.io.data.bits)
    }
    dmaReader.module.io.complete.ready := !dmaReqValid


    /*
     * Write data to buffer
     * dmaWriter.module.io.data.ready will be high for one cycle when DMAWriter writes data
     */
    dmaWriter.module.io.req.bits.address := dmaReqAddress
    dmaWriter.module.io.req.bits.bytesSize := dmaReqBytesSize
    dmaWriter.module.io.req.valid := dmaReqValid && (dmaReqType === DMA_REQ_TYPE_WRITE)

    wen := MuxCase(wen, Seq(
      ((dmaWriter.module.io.req.ready && (dmaReqType === DMA_REQ_TYPE_WRITE)) -> true.B),
      (dmaWriter.module.io.complete.valid -> false.B)))

    dmaWriter.module.io.data.bits := regBuffer(idxW)
    dmaWriter.module.io.data.valid := (dmaReqType === DMA_REQ_TYPE_WRITE)
    when(wen && dmaWriter.module.io.data.ready){
      idxW := Mux(idxW === (bufferSize-1).U, 0.U, idxW + 1.U)
      printf("[HPME Log] idxW=%d, write %d to memory\n", idxW, regBuffer(idxW))
    }
    dmaWriter.module.io.complete.ready := !dmaReqValid

    /*
     * writer and reader send complete signal to controller, then dmaReValid will be clear, thus the writer
     * and the reader will be in s_init state, the transaction completes
     */
    controller.module.io.dmaReq.ready := ((dmaReqType === DMA_REQ_TYPE_READ) && dmaReader.module.io.req.ready) ||
      ((dmaReqType === DMA_REQ_TYPE_WRITE) && dmaWriter.module.io.req.ready)
    controller.module.io.dmaComplete := ((dmaReqType === DMA_REQ_TYPE_READ) && dmaReader.module.io.complete.valid) ||
      ((dmaReqType === DMA_REQ_TYPE_WRITE) && dmaWriter.module.io.complete.valid)
  }
}


// Mount HPME to SoC
// Connect the singals
trait CanHavePeripheryHPME{ this: BaseSubsystem =>
  private val address = BigInt(0x78000000)
  private val portName = "hpme"

  val hpmeOpt = p(HPMEKey).map { params =>
    val hpme = LazyModule(new HPME(HPMEControllerParams(address, pbus.beatBytes))(p))
    pbus.toVariableWidthSlave(Some(portName)){hpme.controller.node}
    fbus.fromPort(Some(portName))() :=* hpme.hpmeNode
    hpme
  }
}

// Implement
trait CanHavePeripheryHPMEModuleImp extends LazyModuleImp{

}
