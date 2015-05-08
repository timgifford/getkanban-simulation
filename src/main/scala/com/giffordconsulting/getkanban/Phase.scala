package com.giffordconsulting.getkanban


class Phase(previous:Phase, next:Phase){

}

object Phase {
  def needsAnalysis(stage:Phase): Boolean = Seq(Ready, AnalysisInProgress).contains(stage)
  def needsDevelopment(stage: Phase) = Seq(AnalysisDone, DevInProgress).contains(stage) || needsAnalysis(stage)
  def needsTesting(stage: Phase): Boolean = Seq(DevDone, Test).contains(stage) || needsDevelopment(stage)


  case object Ready extends Phase(null, AnalysisInProgress)
  case object AnalysisInProgress extends Phase(Ready, AnalysisDone)
  case object AnalysisDone extends Phase (AnalysisInProgress, DevInProgress)
  case object DevInProgress extends Phase(AnalysisDone, DevDone)
  case object DevDone extends Phase(DevInProgress,Test)
  case object Test extends Phase(DevDone, ReadyToDeploy)
  case object ReadyToDeploy extends Phase(Test, Deployed)
  case object Deployed extends Phase(ReadyToDeploy, null)
}

