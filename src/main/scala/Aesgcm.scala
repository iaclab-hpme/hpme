package hpme

import chisel3._
import chisel3.util._
import freechips.rocketchip.subsystem.{BaseSubsystem, CacheBlockBytes}
import freechips.rocketchip.config.{Field, Parameters}
import freechips.rocketchip.diplomacy._
import freechips.rocketchip.regmapper.{HasRegMap, RegField}
import freechips.rocketchip.tilelink._
import freechips.rocketchip.util.UIntIsOneOf
import testchipip.TLHelper
import HPMEConsts._
import aes._

class Aesgcm(implicit p:Parameters) extends LazyModule {
  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle{
      val key = Flipped(Decoupled(UInt(ENC_WIDTH.W)))
      val counter = Flipped(Decoupled(UInt(ENC_WIDTH.W)))
      val reqData = Flipped(Decoupled(Vec(ENC_SIZE, UInt(ENC_WIDTH.W))))
      val respData = Decoupled(Vec(ENC_SIZE, UInt(ENC_WIDTH.W)))
      val respMac = Decoupled(UInt(ENC_WIDTH.W))
      val isEnc = Input(Bool())
    })
    val s_key :: s_keyexp :: s_counter :: s_enc :: s_busy0 :: s_data :: s_busy1 :: s_mac :: s_busy2 :: s_done :: Nil = Enum(10)
    val state = RegInit(s_key)
    val key = Reg(UInt(ENC_WIDTH.W))
    val initCounter = Reg(UInt(ENC_WIDTH.W))
    val iterCounter = Reg(UInt(ENC_WIDTH.W))

//    val reqBuffer = Reg(Vec(BUFFER_SIZE, UInt(BUFFER_WIDTH.W)))
    val respBuffer = Reg(Vec(ENC_SIZE + 1, UInt(ENC_WIDTH.W)))
    val macBuffer = Reg(UInt(ENC_WIDTH.W))

    // aes engine
    val aesEngine = Module(new AesTop(pipelineEng = false, encEngNum = ENGINE_NUM + 1))
    val iterNum = RegInit(0.U(6.W))

    // mac engine
    val macEngine = Module(new calcMac())

    // io ready and valid
    io.key.ready := (state === s_key)
    io.counter.ready := (state === s_counter)
    io.reqData.ready := (state === s_data)
    io.respData.valid := (state === s_done)
    io.respMac.valid := (state === s_done)

    when(io.reqData.fire()){
      for(i <- 0 until ENC_SIZE){
        respBuffer(i + 1) := respBuffer(i + 1) ^ io.reqData.bits(i)
      }
    }
    io.respData.bits := respBuffer.tail
    io.respMac.bits := macBuffer

    // State machine logic
    switch(state){
      is(s_key){
        when(io.key.valid){
          key := io.key.bits
          state := s_keyexp
        }
      }
      is(s_keyexp){
        when(aesEngine.io.startKeyExp){
          state := s_counter
        }
      }
      is(s_counter){
        when(io.counter.valid){
          initCounter := io.counter.bits
          iterCounter := io.counter.bits + 1.U
          state := s_enc
          iterNum := 0.U
        }
      }
      is(s_enc){
        when(iterNum === (ENC_ITER_TIME).U){
          state := s_busy0
        }
        when(aesEngine.io.encIntf.cipher.valid){
          iterNum := iterNum + 1.U
          iterCounter := iterCounter + ENGINE_NUM.U
        }
      }
      is(s_busy0){
        when(aesEngine.io.encIntf.cipher.valid){
          state := s_data
        }
      }
      is(s_data){
        when(io.reqData.valid){
          state := s_busy1
        }
      }
      is(s_busy1){
        state := s_mac
      }
      is(s_mac){
        when(macEngine.io.data.fire()){
          state := s_busy2
        }
      }
      is(s_busy2){
        when(macEngine.io.mac.fire()){
          state := s_done
        }
      }
      is(s_done){
        when(io.respData.fire() && io.respMac.fire()){
          state := s_key
        }
      }
    }

    aesEngine.io.key := key
    aesEngine.io.startKeyExp := (state === s_keyexp && aesEngine.io.keyExpReady)
    aesEngine.io.encIntf.text.valid := (state === s_enc && aesEngine.io.encEngReady)

    when(aesEngine.io.encIntf.cipher.valid === true.B){
      for(i <- 1 until ENC_ITER_TIME){
        for(j <- 1 to ENGINE_NUM){
          respBuffer(i * ENGINE_NUM + j) := respBuffer((i - 1) * ENGINE_NUM + j)
        }
      }
      for(j <- 1 to ENGINE_NUM){
        respBuffer(j) := aesEngine.io.encIntf.cipher.bits(j)
      }
      respBuffer(0) := aesEngine.io.encIntf.cipher.bits(0)
    }

    aesEngine.io.encIntf.text.bits(0) := initCounter
    for(i <- 1 to ENGINE_NUM){
      aesEngine.io.encIntf.text.bits(i) := iterCounter + i.U
    }

    macEngine.io.data.valid := state === s_mac
    macEngine.io.data.bits(0) := respBuffer(0)
    for(i <- 1 to ENC_SIZE){
      macEngine.io.data.bits(i) := Mux(io.isEnc, respBuffer(i), io.reqData.bits(i-1))
    }
    macEngine.io.mac.ready := state === s_busy2

    when(macEngine.io.mac.fire()){
      macBuffer := macEngine.io.mac.bits
    }
  }
}
