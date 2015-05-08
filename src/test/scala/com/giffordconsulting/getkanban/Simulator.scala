package com.giffordconsulting.getkanban

class Simulator() {

  def completeS2 = WorkItems.S2.effort.test = 0


  def applyWorkToTesting(items: Seq[WorkItem], roller: DiceRoller, numberOfResources: Int): Seq[WorkItem] = {
    val itemsWorked = Kanban.testQueue(items)

    var remainingWork: Int = 0

    for (i <- 1 to numberOfResources) remainingWork += roller.roll()

    applyWork(items.last, items.init)

    def applyWork(item: WorkItem, items: Seq[WorkItem]): Unit = {
      if (item.effort.analysis > remainingWork) {
        item.effort.analysis = item.effort.analysis - remainingWork
        remainingWork = 0
      } else {
        remainingWork = remainingWork - item.effort.analysis
        item.effort.analysis = 0
      }

      if (remainingWork != 0) {
        applyWork(items.last, items.init)
      }
    }

    itemsWorked
  }

//  def play(workItems: Seq[WorkItem], diceRoller: DiceRoller = DiceRoller.random, board: Kanban = new Kanban()): Seq[WorkItem] = {
//    applyWorkToTesting(Kanban.testQueue(workItems), diceRoller, 2)
//    new Kanban(workItems).process(WorkDay.Nine)
//    workItems
//  }

}
