package com.giffordconsulting.getkanban

class Simulator() {


  def completeS2 = WorkItems.S2.effort.test = 0

  def play(board: Seq[WorkItem], diceRoller: DiceRoller = DiceRoller.random): Unit = {

    completeS2
    WorkItems.process(board, WorkDay.Nine)
  }

}
