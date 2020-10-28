package hpme

import chisel3._

object HPMEConsts {
  val DMA_REQ_WIDTH = 64
  val DMA_ADDRESS_WIDTH = 64
  val DMA_DATA_WIDTH = 64
  val DMA_DATA_SIZE_WIDTH = 13
  val DMA_MAX_DATA_SIZE = 1 << DMA_DATA_SIZE_WIDTH // maximum data size per request
  val DMA_MAX_BEAT = 256 // maximum amount of beat

  val DMA_REQ_TYPE_READ = 1.U
  val DMA_REQ_TYPE_WRITE = 2.U

  val DMA_REQ_DATA_TYPE_DATA = 0.U
  val DMA_REQ_DATA_TYPE_KEY = 1.U
  val DMA_REQ_DATA_TYPE_MAC = 2.U

  val BUFFER_SIZE = 512
  val BUFFER_WIDTH = 64

  val ENC_REQ_WIDTH = 64
  val ENC_KEY_WIDTH = 128
}
