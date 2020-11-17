package hpme.aes.utils

import chisel3._

class ModIO extends Bundle
  with HasCommonParameters {
  // two operands
  val a = Input(UInt(DataWidth.W))
  val b = Input(UInt(DataWidth.W))
  // modulus
  val m = Input(UInt(DataWidth.W))
  // reduced value
  val c = Output(UInt(DataWidth.W))
}

class ModMulIO extends ModIO {
  val n = Input(UInt(5.W))  // Length 0f modulus
  // precomputed value
  val u = Input(UInt((DataWidth + 2).W))  // (1 << (2 * n + 1)) / m
}

class ModMul extends Module
  with HasCommonParameters {
  val io = IO(new ModMulIO)

  val a = io.a
  val b = io.b
  val m = io.m
  val u = io.u
  val n = io.n

  // process 1
  val c = a * b
  val shift = c >> (n - 2.U)

  // process 2
  val mul1 = u * shift.asUInt()

  // process 3
  val qGuess = mul1 >> (n + 3.U)
  val mul2 = m * qGuess.asUInt()

  // process 4
  val z = c - mul2
  io.c := Mux(z < m, z, z - m)
}

object ModMul {
  def apply(a: UInt, b: UInt, m: UInt, u: UInt, n: UInt): UInt = {
    val inst = Module(new ModMul())
    inst.io.a := a
    inst.io.b := b
    inst.io.m := m
    inst.io.u := u
    inst.io.n := n
    inst.io.c
  }
}