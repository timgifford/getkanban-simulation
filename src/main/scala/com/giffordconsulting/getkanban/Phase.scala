package com.giffordconsulting.getkanban


class Phase(previous:Phase, next:Phase){

}

object Phase {
  def needsAnalysis(stage:Phase): Boolean = Seq(Ready, AnalysisInProcess).contains(stage)
  def needsDevelopment(stage: Phase) = Seq(AnalysisDone, DevInProcess).contains(stage) || needsAnalysis(stage)
  def needsTesting(stage: Phase): Boolean = Seq(DevDone, Test).contains(stage) || needsDevelopment(stage)


  case object Ready extends Phase(null, AnalysisInProcess)
  case object AnalysisInProcess extends Phase(Ready, AnalysisDone)
  case object AnalysisDone extends Phase (AnalysisInProcess, DevInProcess)
  case object DevInProcess extends Phase(AnalysisDone, DevDone)
  case object DevDone extends Phase(DevInProcess,Test)
  case object Test extends Phase(DevDone, ReadyToDeploy)
  case object ReadyToDeploy extends Phase(Test, Deployed)
  case object Deployed extends Phase(ReadyToDeploy, null)

  def calculatePhaseFor(item: WorkItem) : Phase = {
    return Ready
  }
}

