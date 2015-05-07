package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.DiceRoller.allThrees
import com.giffordconsulting.getkanban.Phase.Deployed

class SimulatorSpec extends UnitSpec {

  "play" should "move single item" in {
    val board = WorkItems.day9
    WorkItems.initializeBoard(board)

    new Simulator().play(board, allThrees)

    WorkItems.S2.state should be(Deployed)
  }
}
