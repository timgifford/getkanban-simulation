package com.giffordconsulting.getkanban

import javafx.stage.Stage

import com.giffordconsulting.getkanban.Phase.AnalysisDone
import com.giffordconsulting.getkanban.Phase._
import com.giffordconsulting.getkanban.WorkDay._
import com.giffordconsulting.getkanban.WorkItems._
import org.scalatest.BeforeAndAfter

class WorkItemTest extends UnitSpec with BeforeAndAfter {

  var workItem: WorkItem = Empty
  var wip1: WorkItem = Empty
  var wip2: WorkItem = Empty
  var wip3: WorkItem = Empty
  var wip4: WorkItem = Empty

  before {
    workItem = new WorkItem("ready", new WorkRemaining(1, 1, 1), BusinessValue.High, 3, Ready)
    wip1 = new WorkItem("1", new WorkRemaining(1,1,1), BusinessValue.High, 3, Ready)
    wip2 = new WorkItem("2", new WorkRemaining(1,1,1), BusinessValue.High, 3, Ready)
    wip3 = new WorkItem("3", new WorkRemaining(1,1,1), BusinessValue.High, 3, Ready)
    wip4 = new WorkItem("4", new WorkRemaining(1,1,1), BusinessValue.High, 3, Ready)
  }

  "Day8" should "initialize the board" in {
    initializeBoard(day9)

    S1.state should be(ReadyToDeploy)

    S2.state should be(Test)
    S3.state should be(Test)
    S4.state should be(Test)

    S5.state should be(DevDone)
    S6.state should be(DevInProcess)
    S7.state should be(DevInProcess)
    S8.state should be(DevInProcess)

    S9.state should be(Phase.AnalysisDone)
    S10.state should be(AnalysisInProcess)

    S11.state should be(Ready)
    S12.state should be(Ready)
  }

  "Ready" should "stay in Ready when Analysis at WIP limit" in {
    val items: Seq[WorkItem] = Seq(workItem, wip1, wip2)

    process(items, Nine)

    items.length should be(3)
    workItem.state should be(Ready)
    wip1.state should be(AnalysisInProcess)
    wip2.state should be(AnalysisInProcess)
  }

  it should "move to Analysis In Process" in {
    process(Seq(workItem), Nine)
    workItem.state should be (AnalysisInProcess)
  }
  
  "AnalysisDone" should "move to DevInProcess" in {
    workItem.effort.analysis = 0
    process(Seq(workItem), Ten)
    workItem.state should be(DevInProcess)
  }

  it should "stay in analysisDone when Development at WIP limit" in {
    workItem = buildItem(Phase.AnalysisDone)

    wip1 = buildItem(DevInProcess)
    wip2 = buildItem(DevInProcess)
    wip3 = buildItem(DevInProcess)
    wip4 = buildItem(DevInProcess)

    process(Seq(workItem, wip1, wip2, wip3, wip4), Ten)

    Seq(wip1, wip2,wip3).foreach(_.state should be (DevInProcess))
    workItem.state should be(Phase.AnalysisDone)
  }

  "DevelopmentDone" should "move to Testing" in {
    workItem = buildItem(Phase.DevDone)
    process(Seq(workItem), Ten)
    workItem.state should be (Test)
  }

  def buildEffort(stage: Phase): WorkRemaining = {
    val analysis = if(needsAnalysis(stage)) 3 else 0
    val dev = if(needsDevelopment(stage)) 3 else 0
    val test = if(needsTesting(stage)) 3 else 0
    new WorkRemaining(analysis,dev,test)
  }

  def buildItem(stage: Phase): WorkItem = {
    new WorkItem("", buildEffort(stage), BusinessValue.Med, 1, stage)
  }

  it should "stay in DevDone when Testing at WIP limit" in {
    wip1 = buildItem(Test)
    wip2 = buildItem(Test)
    wip3 = buildItem(Test)
    wip4 = buildItem(Test)
    workItem = buildItem(DevDone)

    process(Seq(workItem, wip1,wip2,wip3,wip4), Ten)

    workItem.state should be(DevDone)
  }

  "ReadyForDeployment" should "not move to deployed when not a release day" in {
    workItem = buildItem(ReadyToDeploy)
    process(Seq(workItem), Ten)
    workItem.state should be (ReadyToDeploy)
  }

  it should "move to deployed on day 9" in {
    workItem = buildItem(ReadyToDeploy)
    process(Seq(workItem), Nine)
    workItem.state should be (Deployed)
  }

  it should "move to deployed on day 12" in {
    workItem = buildItem(ReadyToDeploy)
    process(Seq(workItem), Twelve)
    workItem.state should be (Deployed)
  }

  it should "move to deployed on day 15" in {
    workItem = buildItem(ReadyToDeploy)
    process(Seq(workItem), Fifteen)
    workItem.state should be (Deployed)
  }
}




