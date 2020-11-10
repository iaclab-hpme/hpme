package hpme

import freechips.rocketchip.config.Config

class WithHPME extends Config((site, here, up) => {
  case HPMEKey => Some(HPMEConfig())
})

class WithNoHPME extends Config((site, here, up) => {
  case HPMEKey => None
})
