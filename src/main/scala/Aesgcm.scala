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

class EncReqBundle extends Bundle{
  val data = Vec(BUFFER_SIZE, UInt(BUFFER_WIDTH.W))
  val key = UInt(ENC_KEY_WIDTH.W)
}

class EncRespBundle extends Bundle{
  val data = Vec(BUFFER_SIZE, UInt(BUFFER_WIDTH.W))
}

class Aesgcm(implicit p:Parameters) extends LazyModule {
  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle{
      val req = Flipped(Decoupled(new EncReqBundle))
      val resp = Decoupled(new EncRespBundle)
    })
    val s_init :: s_busy :: s_done :: Nil = Enum(3)
    val state = RegInit(s_init)
    val respBuffer = Reg(Vec(BUFFER_SIZE, UInt(BUFFER_WIDTH.W)))
    io.req.ready := (state === s_init)
    when(state === s_init && io.req.valid){
      for(i <- 0 until BUFFER_SIZE){
        respBuffer(i) := io.req.bits.data(i)
      }
      state := s_busy
    }
    .elsewhen(state === s_busy){
      state := s_done
    }.elsewhen(state === s_done && io.resp.ready){
      state := s_init
    }
    io.resp.bits.data := respBuffer
    io.resp.valid := (state === s_done)

  }
}
