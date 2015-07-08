package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.Phase._

class StoryCardTest extends UnitSpec {
  "Advance" should "move Ready to AnalysisInProgress" in {
    val card: StoryCard = StoryCard(1,1,1, state = Ready).advance(Board())

    card.state should be (AnalysisInProgress)
  }

  it should "move AnalysisDone to DevInProgress" in {
    val card: StoryCard = StoryCard(0,1,1, state = AnalysisDone).advance(Board())

    card.state should be (DevInProgress)
  }

  it should "move DevDone to Test" in {
    val card: StoryCard = StoryCard(0,0,1, state = DevDone).advance(Board())

    card.state should be (Test)
  }
}
