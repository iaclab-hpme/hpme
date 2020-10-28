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
 * DMA Read Node and DMA Write Node are designed independently.
 * They are mounted to FrontBus as Client Nodes.
 */

class DMAReqBundle extends Bundle{
  val address = UInt(DMA_ADDRESS_WIDTH.W)
  val bytesSize = UInt(DMA_DATA_SIZE_WIDTH.W) // transfer bytesSize bytes in total
}

class DMAReader(implicit p:Parameters) extends LazyModule{
  val node = TLHelper.makeClientNode(name="hpmeDMAReader", sourceId=IdRange(0,1))
  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle{
      val req = Flipped(Decoupled(new DMAReqBundle))
      val data = Decoupled(UInt(DMA_DATA_WIDTH.W))
      val complete = Decoupled(Bool())
    })
    val (mem, edge) = node.out(0)

    /*
     * CacheBlockBytes bytes' data can be transferred per request in Tilelink.
     * A large DMA request needs being split to multiple requests.
     * A request is divided into several beats for DMA_DATA_WIDTH bytes' data can be transferred per beat.
     * Memory read state machine is below.
     */
    val s_init :: s_req :: s_read :: s_done :: Nil = Enum(4)
    val state = RegInit(s_init)

    val address = RegInit(0.U(DMA_ADDRESS_WIDTH.W))
    val bytesLeft = RegInit(0.U(DMA_DATA_SIZE_WIDTH.W))
    val blockBytes = p(CacheBlockBytes) // Maximum data transferred per beat

    state := MuxCase(state, Seq(// Keep current state
      (((io.req.valid && (state === s_init)) ||
        // valid when s_init, switch to s_read
        (( bytesLeft > 0.U) && edge.done(mem.d) && (state === s_read))) -> s_req),
      // last beat completed but has bytes left, next beat
      ((edge.done(mem.a) && (state === s_req)) -> s_read),
      // request has been recv by memory when s_req, switch to s_read to start reading
      ((edge.done(mem.d) && (bytesLeft === 0.U) && (state === s_read)) -> s_done),
      // memory has transferred all data, switch to s_done
      ((io.complete.ready && (state === s_done)) -> s_init)))
    // complete signal read, back to s_init
    printf("[HPME log] DMAReader current state: %d, bytesLeft: %d, address: %x\n", state, bytesLeft, address)

    /*
     * Respond to read requests and send read memory requests
     */
    io.req.ready := (state === s_init)
    bytesLeft := MuxCase(bytesLeft, Seq(
      ((state === s_init) -> io.req.bits.bytesSize),
      (edge.done(mem.a) -> (bytesLeft - blockBytes.U))))
    address := MuxCase(address, Seq(
      ((state === s_init) -> io.req.bits.address),
      (edge.done(mem.a) -> (address + blockBytes.U))))

    // Send req and get data when s_read
    mem.a.bits := edge.Get(
      fromSource = 0.U,
      toAddress = address,
      lgSize = log2Ceil(blockBytes).U)._2
    mem.a.valid := (state === s_req)

    /*
     * fetch data to buffer
     */
    io.data.bits := mem.d.bits.data
    mem.d.ready := io.data.ready
    val dChannelCount = edge.count(mem.d)._4
    val preBeat = RegInit(DMA_MAX_BEAT.U)
    io.data.valid := mem.d.valid && (state === s_read) && (preBeat =/= dChannelCount)
    preBeat := Mux(io.data.ready, dChannelCount, preBeat)

    /*
     * Output complete signals
     */
    io.complete.valid := (state === s_done)
    io.complete.bits := (state === s_done)
  }
}


class DMAWriter(implicit p:Parameters) extends LazyModule{
  val node = TLHelper.makeClientNode(name="hpmeDMAWriter", sourceId=IdRange(0,1))
  lazy val module = new LazyModuleImp(this){
    val io = IO(new Bundle{
      val req = Flipped(Decoupled(new DMAReqBundle))
      val data = Flipped(Decoupled(UInt(DMA_DATA_WIDTH.W)))
      val complete = Decoupled(Bool())
    })

    val (mem, edge) = node.out(0)
    val s_init::s_write::s_resp::s_done::Nil = Enum(4)
    val state = RegInit(s_init)

    val address = RegInit(0.U(DMA_ADDRESS_WIDTH.W))
    val bytesLeft = RegInit(0.U(DMA_DATA_SIZE_WIDTH.W))
    val blockBytes = p(CacheBlockBytes)

    state := MuxCase(state, Seq(
      ((((state === s_init) && io.req.valid) ||
        ((state === s_resp) && edge.done(mem.d) && (bytesLeft > 0.U))) -> s_write),
      (((state === s_write) && edge.done(mem.a)) -> s_resp),
      (((state === s_resp) && mem.d.fire() && (bytesLeft === 0.U)) -> s_done),
      (((state === s_done) && io.complete.ready) -> s_init)))

    io.req.ready := (state === s_init)
    bytesLeft := MuxCase(bytesLeft, Seq(
      ((state === s_init) -> io.req.bits.bytesSize),
      (edge.done(mem.a) -> (bytesLeft - blockBytes.U))))
    address := MuxCase(address, Seq(
      ((state === s_init) -> io.req.bits.address),
      (edge.done(mem.a) -> (address + blockBytes.U))))

    mem.d.ready := state === s_resp
    mem.a.valid := (state === s_write) && (io.data.valid)
    mem.a.bits := edge.Put(
      fromSource = 0.U,
      toAddress = address,
      lgSize = log2Ceil(blockBytes).U,
      data = io.data.bits)._2

    io.data.ready := mem.a.fire()
    printf("[HPME Log] DMAWriter current state: %d, bytesLeft: %d, address: %x, mem.a.fire: %d\n",
      state, bytesLeft, address, mem.a.fire())

    io.complete.valid := (state === s_done)
    io.complete.bits := (state === s_done)
  }
}