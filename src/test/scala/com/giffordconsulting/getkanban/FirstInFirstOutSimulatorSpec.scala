package com.giffordconsulting.getkanban

abstract class SimulatorSpec extends UnitSpec{

  def createSimulation(): Simulation

  val simulation = createSimulation()

  "Day 8" should "have 12 items in play" in {
    simulation.endOfDay(8).cards.size should be (12)
  }

  it should "have no revenue" in {
    simulation.endOfDay(8).revenue should be (0)
  }

}

class BusinessValueDividedByDurationSpec extends SimulatorSpec {
  override def createSimulation(): Simulation = {
    new Simulation(new BusinessValueDividedByDuration).playUntilDay(15)
  }

  "Day 9" should "have some revenue" in {
    simulation.endOfDay(9).revenue should be (110)
  }

  "Day 12" should "have some revenue" in {
    simulation.endOfDay(12).revenue should be (800)
  }

  "Day 15" should "have some revenue" in {
    simulation.endOfDay(15).revenue should be (1820)
  }
}

class BusinessValueStrategySpec extends SimulatorSpec {
  override def createSimulation(): Simulation = {
    new Simulation(new BusinessValueStrategy).playUntilDay(15)
  }

  "Day 9" should "have some revenue" in {
    simulation.endOfDay(9).revenue should be (170)
  }

  "Day 12" should "have some revenue" in {
    simulation.endOfDay(12).revenue should be (980)
  }

  "Day 15" should "have some revenue" in {
    simulation.endOfDay(15).revenue should be (1440)
  }
}

class LowestRemainingWorkSimulatorSpec extends SimulatorSpec {
  override def createSimulation(): Simulation = {
    new Simulation(new LowestRemainingEffortFirst).playUntilDay(15)
  }

  "Day 9" should "have 230 in revenue" in {
    simulation.endOfDay(9).revenue should be (230)
  }

  "Day 12" should "have 920 in revenue" in {
    simulation.endOfDay(12).revenue should be (920)
  }

  "Day 15" should "have 1620 in revenue" in {
    simulation.endOfDay(15).revenue should be (1620)
  }
}

class FirstInFirstOutSimulatorSpec extends SimulatorSpec {

  override def createSimulation(): Simulation ={
    new Simulation(new FirstInFirstOut).playUntilDay(15)
  }

  "Day 9" should "have revenue of 110" in {
    simulation.endOfDay(9).revenue should be (110)
  }

  it should "have 12 items in play" in {
    simulation.endOfDay(9).cards.size should be (12)
  }

  it should "have empty Ready to Deploy queue" in {
    simulation.endOfDay(9).cardsInPhase(Phase.ReadyToDeploy).size should be (0)
  }

  "Day 10" should "have 12 items in play" in {
    simulation.endOfDay(10).cards.size should be (12)
  }

  it should "have 110 in revenue" in {
    simulation.endOfDay(10).revenue should be (110)
  }

  "Day 12" should "have 1010 in revenue" in {
    simulation.endOfDay(12).revenue should be (1010)
  }

  "Day 15" should "have 1750 in revenue" in {
    simulation.endOfDay(15).revenue should be (1750)
  }

}
