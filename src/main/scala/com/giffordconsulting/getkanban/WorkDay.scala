package com.giffordconsulting.getkanban

case class WorkDay(val index: Int, multiplier: Int, next: WorkDay) {
  def isDeployDay = Seq(9, 12, 15).contains(index)
}

object WorkDay {
  object Eight extends WorkDay(8,0, Nine)
  object Nine extends WorkDay(9, 10, Ten)
  object Ten extends WorkDay(10,0, Eleven)
  object Eleven extends WorkDay(11,0,Twelve)
  object Twelve extends WorkDay(12, 15, Thirteen)
  object Thirteen extends WorkDay(13, 15, Fourteen)
  object Fourteen extends WorkDay(13, 15, Fifteen)
  object Fifteen extends WorkDay(15, 20, null)
}