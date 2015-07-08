package com.giffordconsulting.getkanban


case class Phase(val value: Int) extends AnyVal with Ordered[Phase] {
  override def compare(that: Phase): Int = this.value.compare(that.value)
}

object Phase{
  val Ready = Phase(0)
  val AnalysisInProgress = Phase(1)
  val AnalysisDone = Phase(2)       // TODO: Doesn't move
  val DevInProgress = Phase(3)
  val DevDone = Phase(4)            // TODO: Doesn't move
  val Test = Phase(5)
  val ReadyToDeploy = Phase(6)
  val Deployed = Phase(7)

}

