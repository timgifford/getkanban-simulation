package com.giffordconsulting.getkanban

class LowestRemainingWorkForCurrentQueueTest extends UnitSpec {

  "Sort" should "have card with least work for current queue" in {
    val cards = Seq(
      StoryCard(10,10,10, state = Phase.AnalysisInProgress, name = "ten points"),
      StoryCard(1,100,100, state = Phase.AnalysisInProgress, name="one point")
    ).sorted(new LowestRemainingWorkForCurrentQueue).groupBy(x => Phase.AnalysisInProgress).getOrElse(Phase.AnalysisInProgress, Seq.empty)

    cards.head.name should be("one point")
    cards.last.name should be("ten points")
  }

  it should "not sort if already in order" in {
    val cards = Seq(
      StoryCard(1,100,100, state = Phase.AnalysisInProgress, name="one point"),
      StoryCard(10,1000,1000, state = Phase.AnalysisInProgress, name = "ten points")
    ).sorted(new LowestRemainingWorkForCurrentQueue).groupBy(x => Phase.AnalysisInProgress).getOrElse(Phase.AnalysisInProgress, Seq.empty)

    cards.head.name should be("one point")
    cards.last.name should be("ten points")
  }
}
