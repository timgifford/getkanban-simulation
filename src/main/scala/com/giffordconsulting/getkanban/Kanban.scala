package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.Phase._

class Kanban(var items: Seq[WorkItem] = Seq()) {
  def process(day: WorkDay) = {
    for(i <- items.reverse){
      processItem(i, day)
    }
  }

  def initializeBoard() = {
    for(i <- items){
      processItem(i, WorkDay.Eight)
    }
  }

  def processItem(item: WorkItem, day: WorkDay): Unit = {
    if (item.needsAnalysis && spaceAvailableInAnalysis(item)) {
      item.state = AnalysisInProgress
    }
    if(!item.needsAnalysis){
      item.state = AnalysisDone
    }
    if (!item.needsAnalysis && spaceAvailableInDevelopment(item)) {
      item.state = DevInProgress
    }
    if(!item.needsDevelopment ){
      item.state = DevDone
    }
    if (!item.needsDevelopment && spaceAvailableInTesting(item)) {
      item.state = Test
    }
    if (!item.needsTesting) {
      item.state = ReadyToDeploy
    }
    if (day.isDeployDay && item.state == ReadyToDeploy) {
      item.state = Deployed
    }
  }

  def spaceAvailableInDevelopment(item: WorkItem): Boolean = {
    (developerQueue.length < 4 || developerQueue.contains(item))
  }

  def spaceAvailableInTesting(item: WorkItem): Boolean = {
    (testQueue.length < 3 || testQueue.contains(item))
  }

  def spaceAvailableInAnalysis(item: WorkItem): Boolean = {
    analysisQueue.length < 2
  }

  def testQueue(): Seq[WorkItem] = {
    items.filter(x => x.state == Test)
  }

  def developerQueue(): Seq[WorkItem] = {
    items.filter(x => x.state == DevInProgress || x.state == DevDone)
  }

  def analysisQueue(): Seq[WorkItem] = {
    items.filter(x => x.state == AnalysisDone || x.state == AnalysisInProgress)
  }

}

object Kanban {
  def testQueue(items: Seq[WorkItem]): Seq[WorkItem] = {
    items.filter(x => x.state == Test)
  }

  def developerQueue(items: Seq[WorkItem]): Seq[WorkItem] = {
    items.filter(x => x.state == DevInProgress || x.state == DevDone)
  }

  def analysisItem(items: Seq[WorkItem]): Seq[WorkItem] = {
    items.filter(x => x.state == AnalysisDone || x.state == AnalysisInProgress)
  }

}
