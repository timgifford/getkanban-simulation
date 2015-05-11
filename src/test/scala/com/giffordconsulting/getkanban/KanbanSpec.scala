package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.Phase._
import com.giffordconsulting.getkanban.WorkDay._
import com.giffordconsulting.getkanban.WorkItems._
import org.scalatest.BeforeAndAfter



class KanbanSpec extends UnitSpec with BeforeAndAfter {

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

  "Kanban" should "initialize the board" in {
    new Kanban(day9).initializeBoard()

    S1.state should be(ReadyToDeploy)

    S2.state should be(Test)
    S3.state should be(Test)
    S4.state should be(Test)

    S5.state should be(DevDone)
    S6.state should be(DevInProgress)
    S7.state should be(DevInProgress)
    S8.state should be(DevInProgress)

    S9.state should be(Phase.AnalysisDone)
    S10.state should be(AnalysisInProgress)

    S11.state should be(Ready)
    S12.state should be(Ready)
  }

  "Ready" should "stay in Ready when Analysis at WIP limit" in {
    val items: Seq[WorkItem] = Seq(workItem, wip1, wip2)

    new Kanban(items).process(Nine)

    items.length should be(3)
    workItem.state should be(Ready)
    wip1.state should be(AnalysisInProgress)
    wip2.state should be(AnalysisInProgress)
  }

  it should "move to Analysis In Process" in {
    new Kanban(Seq(workItem)).process(Nine)
    workItem.state should be (AnalysisInProgress)
  }
  
  "AnalysisDone" should "move to DevInProcess" in {
    workItem.effort.analysis = 0
    new Kanban(Seq(workItem)).process(Ten)
    workItem.state should be(DevInProgress)
  }

//  it should "stay in analysisDone when Development at WIP limit" in {
//    workItem = buildItem(Phase.AnalysisDone)
//
//    wip1 = buildItem(DevInProgress)
//    wip2 = buildItem(DevInProgress)
//    wip3 = buildItem(DevInProgress)
//    wip4 = buildItem(DevInProgress)
//
//    new Kanban(Seq(workItem, wip1, wip2, wip3, wip4)).process(Ten)
//
//    Seq(wip1, wip2,wip3).foreach(_.state should be (DevInProgress))
//    workItem.state should be(Phase.AnalysisDone)
//  }
//
//  "DevelopmentDone" should "move to Testing" in {
//    workItem = buildItem(Phase.DevDone)
//    new Kanban(Seq(workItem)).process(Ten)
//    workItem.state should be (Test)
//  }

//  def buildEffort(stage: Phase): WorkRemaining = {
//    val analysis = if(needsAnalysis(stage)) 3 else 0
//    val dev = if(needsDevelopment(stage)) 3 else 0
//    val test = if(needsTesting(stage)) 3 else 0
//    new WorkRemaining(analysis,dev,test)
//  }

//  def buildItem(stage: Phase): WorkItem = {
//    new WorkItem("", buildEffort(stage), BusinessValue.Med, 1, stage)
//  }

//  it should "stay in DevDone when Testing at WIP limit" in {
//    wip1 = buildItem(Test)
//    wip2 = buildItem(Test)
//    wip3 = buildItem(Test)
//    wip4 = buildItem(Test)
//    workItem = buildItem(DevDone)
//
//    new Kanban(Seq(workItem, wip1,wip2,wip3,wip4)).process(Ten)
//
//    workItem.state should be(DevDone)
//  }
//
//  "ReadyForDeployment" should "not move to deployed when not a release day" in {
//    workItem = buildItem(ReadyToDeploy)
//    new Kanban(Seq(workItem)).process(Ten)
//    workItem.state should be (ReadyToDeploy)
//  }
//
//  it should "move to deployed on day 9" in {
//    workItem = buildItem(ReadyToDeploy)
//    new Kanban(Seq(workItem)).process(Nine)
//    workItem.state should be (Deployed)
//  }
//
//  it should "move to deployed on day 12" in {
//    workItem = buildItem(ReadyToDeploy)
//    new Kanban(Seq(workItem)).process(Twelve)
//    workItem.state should be (Deployed)
//  }
//
//  it should "move to deployed on day 15" in {
//    workItem = buildItem(ReadyToDeploy)
//    new Kanban(Seq(workItem)).process(Fifteen)
//    workItem.state should be (Deployed)
//  }
}




