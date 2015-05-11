package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.Phase.Ready

class PhaseTest extends UnitSpec {

  "Ready" should "less than Deployed" in {
    Ready should be < Phase.Deployed
  }

  it should "equal Ready" in {
    Ready should be (Ready)
  }

}
