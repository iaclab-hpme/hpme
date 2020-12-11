package hpme.aes.utils

import chisel3._
import chisel3.util._

class ModIO(val DataWidth: Int) extends Bundle {
  // two operands
  val a = Input(UInt(DataWidth.W))
  val b = Input(UInt(DataWidth.W))
  // modulus
  val m = Input(UInt(DataWidth.W))
  // reduced value
  val c = Output(UInt(DataWidth.W))
}

class ModMulIO(DataWidth: Int) extends ModIO(DataWidth) {
  // precomputed value
  val u = Input(UInt((DataWidth + 2).W))
}

class ModAdd(DataWidth: Int) extends Module {
  val io = IO(new ModIO(DataWidth))

  val ctmp1 = Wire(UInt((DataWidth + 1).W))
  ctmp1 := io.a +& io.b
  val ctmp2 = Wire(UInt((DataWidth + 1).W))
  ctmp2 := ctmp1 -& io.m
  val flag = ctmp2(DataWidth)
  io.c := Mux(flag, ctmp1(DataWidth - 1, 0), ctmp2(DataWidth - 1, 0))
}

object ModAdd {
  def apply(DataWidth: Int)(a: UInt, b: UInt, m: UInt): UInt = {
    val inst = Module(new ModAdd(DataWidth))
    inst.io.a := a
    inst.io.b := b
    inst.io.m := m
    inst.io.c
  }
}

class ModSub(DataWidth: Int) extends Module{
  val io = IO(new ModIO(DataWidth))

  val ctmp1 = Wire(UInt((DataWidth + 1).W))
  ctmp1 := io.a -& io.b
  val ctmp2 = Wire(UInt((DataWidth + 1).W))
  ctmp2 := ctmp1 +& io.m
  val flag = ctmp1(DataWidth)
  io.c := Mux(flag, ctmp2(DataWidth - 1, 0), ctmp1(DataWidth - 1, 0))
}

object ModSub {
  def apply(DataWidth: Int)(a: UInt, b: UInt, m: UInt): UInt = {
    val inst = Module(new ModSub(DataWidth))
    inst.io.a := a
    inst.io.b := b
    inst.io.m := m
    inst.io.c
  }
}

class ModMul(DataWidth: Int) extends Module {
  val io = IO(new ModMulIO(DataWidth))

  val a = io.a
  val b = io.b
  val m = io.m
  val u = io.u

  // pipeline 1
  val c = a * b
  val shift = (c >> (DataWidth - 2).U)(DataWidth + 1, 0)
  val shiftd1 = RegNext(shift)
  val ud1 = RegNext(u)

  // pipeline 2
  val mul1 = (ud1 * shiftd1.asUInt())(2 * DataWidth + 2, 0)
  val mul1d2 = RegNext(mul1)

  // pipeline 3
  val qGuess = (mul1d2 >> (DataWidth + 3).U)(DataWidth, 0)
  val mul2 = (io.m * qGuess.asUInt())(2 * DataWidth - 1, 0)
  val cd3 = ShiftRegister(c, 3)
  val mul2d3 = RegNext(mul2)

  // pipeline 4
  val z = cd3 - mul2d3
  io.c := RegNext(Mux(z < io.m, z, z - io.m))
}

object ModMul {
  def apply(DataWidth: Int)(a: UInt, b: UInt, m: UInt, u: UInt): UInt = {
    val inst = Module(new ModMul(DataWidth))
    inst.io.a := a
    inst.io.b := b
    inst.io.m := m
    inst.io.u := u
    inst.io.c
  }
}

class ModHalf(DataWidth: Int) extends Module {
  val io = IO(new Bundle {
    val a = Input(UInt(DataWidth.W))
    val m = Input(UInt(DataWidth.W))
    val mHalf = Input(UInt(DataWidth.W))
    // reduced value
    val c = Output(UInt(DataWidth.W))
  })
  val even = (io.a >> 1.U).asUInt()
  val odd = ModAdd(DataWidth)(even, io.mHalf, io.m)
  io.c := Mux(io.a(0), odd, even)
}

object ModHalf {
  def apply(DataWidth: Int)(a: UInt, m: UInt, mHalf: UInt): UInt = {
    val inst = Module(new ModHalf(DataWidth))
    inst.io.a := a
    inst.io.m := m
    inst.io.mHalf := mHalf
    inst.io.c
  }
}