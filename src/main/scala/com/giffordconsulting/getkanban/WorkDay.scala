package com.giffordconsulting.getkanban

case class WorkDay(index: Int) {
  def next(): WorkDay = WorkDay(index + 1)

  def isDeployDay = Seq(9, 12, 15).contains(index)
}

object WorkDay {

  object Nine extends WorkDay(9)

  object Eight extends WorkDay(8)

  object Ten extends WorkDay(10)

  object Twelve extends WorkDay(12)

  object Fifteen extends WorkDay(15)

}