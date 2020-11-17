package hpme.aes

import chisel3._
import chisel3.util._
import hpme.HPMEConsts._
import utils._

class calcMac extends Module{
  val io = IO(new Bundle{
    val data = Flipped(Decoupled(Vec(ENC_SIZE + 1, UInt(ENC_WIDTH.W))))
    val mac = Decoupled(UInt(ENC_WIDTH.W))
  })
  val dataReady = RegInit(false.B)
  io.data.ready := dataReady
  val macValid = RegInit(false.B)
  io.mac.valid := macValid
  val mac = Reg(UInt(ENC_WIDTH.W))
  io.mac.bits := mac

  val idle :: running :: done :: Nil = Enum(3)
  val fsm = RegInit(idle)

  val regs = Reg(Vec(ENC_SIZE + 1, UInt(ENC_WIDTH.W)))
  val index = RegInit(1.U(16.W))

  val mod = BigInt("123456789abcdef0123456789abcdef0", 16)
  val u = BigInt("1" + "0" * (2*ENC_WIDTH + 1), 2) / mod


  switch(fsm){
    is(idle){
      macValid := false.B
      dataReady := true.B
      when(io.data.valid){
        for(i <- 0 until ENC_SIZE + 1){
          regs(i) := io.data.bits(i)
        }
        fsm := running
        printf("[calcMac] idle end\n")
      }
    }
    is(running){
      dataReady := false.B
      when(index < ENC_SIZE.U){
        regs(index + 1.U) := regs(index + 1.U) ^ utils.ModMul(regs(index), regs(0), mod.U, u.U, ENC_WIDTH.U)
        index := index + 1.U
      }.otherwise {
        mac := regs(0) ^ utils.ModMul(regs(index), regs(0), mod.U, u.U, ENC_WIDTH.U)
        index := 1.U
        fsm := done
        printf("[calcMac] running end\n")
      }
    }
    is(done){
      when(io.mac.ready){
        macValid := true.B
        fsm := idle
        printf("[calcMac] done end\n")
      }
    }
  }
}
