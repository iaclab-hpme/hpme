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

case class HPMEConfig(
                     base: BigInt,
                     portName: String,
                     devName: String
                     )

case object HPMEKey extends Field[Option[HPMEConfig]](None)

class HPME(c: HPMEControllerParams)(implicit p:Parameters) extends LazyModule {
  val controller = LazyModule(new HPMEController(c))
  val dmaReader = LazyModule(new DMAReader)
  val dmaWriter = LazyModule(new DMAWriter)
  val aesgcm = LazyModule(new Aesgcm)

  val hpmeNode = TLIdentityNode()
  hpmeNode := TLWidthWidget(8) := dmaReader.node // bus with 8-bytes width
  hpmeNode := TLWidthWidget(8) := dmaWriter.node

  lazy val module = new LazyModuleImp(this) {
    val ren = RegInit(false.B) // read enable
    val wen = RegInit(false.B) // write enable
    val dmaReqType = RegInit(0.U(2.W)) // 1.U: read req, 2.U: write req

    val s_idle :: s_key :: s_counter :: s_data :: s_busy :: s_done :: Nil = Enum(6)
    val state = RegInit(s_idle)

    /*
     * create and init data buffer and helper vars
     */
    val bufferSize = BUFFER_SIZE // buffer is 512 * 64bit
    val bufferSizeBits = log2Ceil(bufferSize)
    val initSeq = for(i <- 1 until (bufferSize + 1)) yield 0.U(BUFFER_WIDTH.W)

    val readBuffer = RegInit(VecInit(initSeq))
    val writeBuffer = RegInit(VecInit(initSeq))
    val keyBuffer = Reg(UInt(ENC_WIDTH.W))
    val cntBuffer = Reg(UInt(ENC_WIDTH.W))
    val macBuffer = Reg(UInt(ENC_WIDTH.W))

    val idxR = RegInit(0.U(bufferSizeBits.W))
    val idxW = RegInit(0.U(bufferSizeBits.W))

    /*
     * Decode DMA request
     */
    dmaReqType := controller.module.io.dmaReq.bits(1,0)
    val dmaReqAddress = Cat(0.U(16), controller.module.io.dmaReq.bits(49,2))
    val dmaReqBytesSize = controller.module.io.dmaReq.bits(60, 50) << 2
    val dmaReqDataType = controller.module.io.dmaReq.bits(62, 61)
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
    val dmaReadDone = ren && dmaReader.module.io.data.valid
    when(dmaReadDone){ // valid will be high for one cycle when data comes
      switch(dmaReqDataType){
        is(DMA_REQ_DATA_TYPE_DATA){
          readBuffer(idxR) := dmaReader.module.io.data.bits
          idxR := Mux(idxR === (bufferSize-1).U, 0.U, idxR + 1.U)
        }
//        is(DMA_REQ_DATA_TYPE_KEY){
//          keyBuffer(idxR) := dmaReader.module.io.data.bits
//          idxR := Mux(idxR === 1.U, 0.U, 1.U)
//        }
//        is(DMA_REQ_DATA_TYPE_CNT){
//          cntBuffer(idxR) := dmaReader.module.io.data.bits
//          idxR := Mux(idxR === 1.U, 0.U, 1.U)
//        }
      }
      dmaReader.module.io.data.ready := true.B
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

    dmaWriter.module.io.data.bits := writeBuffer(idxW)
    dmaWriter.module.io.data.valid := (dmaReqType === DMA_REQ_TYPE_WRITE)
    when(wen && dmaWriter.module.io.data.ready){
      idxW := Mux(idxW === (bufferSize-1).U, 0.U, idxW + 1.U)
      printf("[HPME Log] idxW=%d, write %d to memory\n", idxW, writeBuffer(idxW))
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
    controller.module.io.hpmeIsIdle := state === s_idle
    when(controller.module.io.dmaComplete){
      idxR := 0.U
      idxW := 0.U
    }


    controller.module.io.key.ready := state === s_idle
    controller.module.io.counter.ready := state === s_idle

    when(controller.module.io.key.fire()){
      keyBuffer := controller.module.io.key.bits
    }
    when(controller.module.io.counter.fire()){
      cntBuffer := controller.module.io.counter.bits
    }





    /*
     * FSM logic
     */
    switch(state){
      is(s_idle){
        state := MuxCase(state, Seq(
          ((dmaReqType === DMA_REQ_TYPE_READ) && dmaReader.module.io.complete.valid) -> s_data,
          (controller.module.io.key.fire()) -> s_key,
          (controller.module.io.counter.fire()) -> s_counter,
        ))
      }
      is(s_key){
        when(aesgcm.module.io.key.fire()){
          state := s_idle
        }
      }
      is(s_counter){
        when(aesgcm.module.io.counter.fire()){
          state := s_idle
        }
      }
      is(s_data){
        when(aesgcm.module.io.reqData.fire()){
          state := s_busy
        }
      }
      is(s_busy){
        when(aesgcm.module.io.respData.fire()){
          state := s_done
        }
      }
      is(s_done){
        when(dmaReqType === DMA_REQ_TYPE_NONE){
          state := s_idle
        }
      }
    }

    /*
     * aesgcm signal logic
     */
    aesgcm.module.io.key.bits := keyBuffer
    aesgcm.module.io.counter.bits := cntBuffer
    for(i <- 0 until ENC_SIZE){
      aesgcm.module.io.reqData.bits(i) := Cat(readBuffer(i*2 + 1), readBuffer(i*2))
    }

    aesgcm.module.io.key.valid := state === s_key
    aesgcm.module.io.counter.valid := state === s_counter
    aesgcm.module.io.reqData.valid := state === s_data
    aesgcm.module.io.isEnc := controller.module.io.isEnc

    aesgcm.module.io.respData.ready := state === s_busy
    aesgcm.module.io.respMac.ready := state === s_busy

    when(aesgcm.module.io.respData.fire()){
      for(i <- 0 until ENC_SIZE){
        writeBuffer(i*2) := aesgcm.module.io.respData.bits(i)(63,0)
        writeBuffer(i*2 + 1) := aesgcm.module.io.respData.bits(i)(127,64)
      }
    }
    when(aesgcm.module.io.respMac.fire()){
      macBuffer := aesgcm.module.io.respMac.bits
    }
    controller.module.io.mac.bits := macBuffer
    controller.module.io.mac.valid := RegNext(state === s_done)
  }
}


// Mount HPME to SoC
// Connect the singals
trait CanHavePeripheryHPME{ this: BaseSubsystem =>

  val hpmeOpt = p(HPMEKey).map { params =>
    val hpme = LazyModule(new HPME(HPMEControllerParams(params.base, pbus.beatBytes, params.devName))(p))
    pbus.toVariableWidthSlave(Some(params.portName)){hpme.controller.node}
    fbus.fromPort(Some(params.portName))() :=* hpme.hpmeNode
    hpme
  }
}

// Implement
trait CanHavePeripheryHPMEModuleImp extends LazyModuleImp{

}
