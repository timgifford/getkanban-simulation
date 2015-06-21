package com.giffordconsulting.getkanban

class WorkDaySpec extends UnitSpec {
  "Next" should "increment the day" in {
    WorkDay(8).next() should be (WorkDay(9))
  }

  "Deploy" should "happen on day 9, 12, 15" in {
    WorkDay(9).isDeployDay should be (true)
    WorkDay(12).isDeployDay should be (true)
    WorkDay(15).isDeployDay should be (true)
  }

}
