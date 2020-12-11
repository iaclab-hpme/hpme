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

  val idle :: prepare :: running :: done :: Nil = Enum(4)
  val fsm = RegInit(idle)

  val regs = Reg(Vec(ENC_SIZE + 1, UInt(ENC_WIDTH.W)))
  val index = RegInit(1.U(16.W))
  val regWriteIdx = RegInit(0.U(16.W))
  val regWriteEnable = RegInit(false.B)

  val mod = BigInt("c0000000000000000000000000000001", 16)
  val u = BigInt("1" + "0" * (2*ENC_WIDTH + 1), 2) / mod

  val modMulA = Wire(UInt(ENC_WIDTH.W))
  val modMulB = Wire(UInt(ENC_WIDTH.W))

  val modMulResult = utils.ModMul(ENC_WIDTH)(modMulA, modMulB, mod.U, u.U)
  modMulA := Mux(index < (ENC_SIZE + 1).U, regs(index), regs(0))
  modMulB := regs(0)


  switch(fsm){
    is(idle){
      macValid := false.B
      dataReady := true.B
      index := 1.U
      when(io.data.valid){
        for(i <- 0 until ENC_SIZE + 1){
          regs(i) := io.data.bits(i)
        }
        fsm := prepare
        printf("[calcMac] idle end\n")
      }
    }
    is(prepare){ // wait 4 cycles until modmul pipeline is full
      dataReady := false.B
      when(index === 4.U){
        fsm := running
      }
      index := index + 1.U
    }
    is(running){
      index := index + 1.U
      when(index < (ENC_SIZE + 3).U){
        regs(index - 3.U) := regs(index - 3.U) ^ modMulResult
      }.otherwise {
        mac := regs(0) ^ modMulResult
        fsm := done
        printf("[calcMac] running end\n")
      }
    }
    is(done){
      macValid := true.B
      when(io.mac.fire()){
        fsm := idle
        printf("[calcMac] done end\n")
      }
    }
  }
}
