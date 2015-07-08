package com.giffordconsulting.getkanban

class WorkDaySpec extends UnitSpec {
  "Next" should "increment the day" in {
    WorkDay.Eight.next should be (WorkDay.Nine)
  }

  "Deploy" should "happen on day 9, 12, 15" in {
    WorkDay.Nine.isDeployDay should be (true)
    WorkDay.Twelve.isDeployDay should be (true)
    WorkDay.Fifteen.isDeployDay should be (true)
  }

}
