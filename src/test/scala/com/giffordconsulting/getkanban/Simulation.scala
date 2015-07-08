package com.giffordconsulting.getkanban

import scala.collection.mutable

class Simulation(prioritizationStrategy: Ordering[StoryCard]) {
  def playUntilDay(stopDay: Int): Simulation = {
    val startingBoard: Board = Board.startingLayout(prioritizationStrategy)
    endOfDay += (8 -> startingBoard)
//    println(8, startingBoard)

    for(playDay <- 9 to stopDay){
      val daysBoard: Board = endOfDay(playDay - 1).play(Points(10, 15, 10))
//      println(playDay, daysBoard)
      endOfDay += (playDay -> daysBoard)
    }
    this
  }

  var endOfDay = new mutable.HashMap[Int, Board]()
}
