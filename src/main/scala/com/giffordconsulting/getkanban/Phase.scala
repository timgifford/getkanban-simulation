package com.giffordconsulting.getkanban


case class Phase(val value: Int) extends AnyVal with Ordered[Phase] {
  //  def needsAnalysis(stage:Phase): Boolean = Seq(Ready, AnalysisInProgress).contains(stage)
//  def needsDevelopment(stage: Phase) = Seq(AnalysisDone, DevInProgress).contains(stage) || needsAnalysis(stage)
//  def needsTesting(stage: Phase): Boolean = Seq(DevDone, Test).contains(stage) || needsDevelopment(stage)

//  def Ready = 0 //extends Phase(null, AnalysisInProgress)

//  case object AnalysisInProgress extends Phase(Ready, AnalysisDone)
//  case object AnalysisDone extends Phase (AnalysisInProgress, DevInProgress)
//  case object DevInProgress extends Phase(AnalysisDone, DevDone)
//  case object DevDone extends Phase(DevInProgress,Test)
//  case object Test extends Phase(DevDone, ReadyToDeploy)
//  case object ReadyToDeploy extends Phase(Test, Deployed)
//  case object Deployed extends Phase(ReadyToDeploy, null)
  override def compare(that: Phase): Int = this.value.compare(that.value)
}

object Phase{
  val Ready = Phase(0)
  val AnalysisInProgress = Phase(1)
  val AnalysisDone = Phase(2)
  val DevInProgress = Phase(3)
  val DevDone = Phase(4)
  val Test = Phase(5)
  val ReadyToDeploy = Phase(6)
  val Deployed = Phase(7)
}

