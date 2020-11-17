package hpme

import freechips.rocketchip.config.Config

class WithHPME(base: BigInt = 0x78000000L, portName: String = "hpme", devName: String = "hpmeController")
  extends Config((site, here, up) => {
  case HPMEKey => Some(HPMEConfig(base, portName, devName))
})

class WithNoHPME extends Config((site, here, up) => {
  case HPMEKey => None
})
