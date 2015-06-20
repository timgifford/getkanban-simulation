package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.Phase._

case class StoryCard(val analysisRemaining: Int, val developmentRemaining: Int, val testingRemaining: Int, val state: Phase = Ready) {
  def applyAnalysisAndReturnRemainder(points: Int): (StoryCard, Int) = {
    var remainingPoints: Int = 0
    if(points > analysisRemaining){
      remainingPoints = points - analysisRemaining
      (this.copy(analysisRemaining = 0), remainingPoints)
    }
    else{
      (this.copy(analysisRemaining = this.analysisRemaining - points), 0)
    }
  }

  def applyDevAndReturnRemainder(points: Int): (StoryCard, Int) = {
    var remainingPoints: Int = 0
    if(points > developmentRemaining){
      remainingPoints = points - developmentRemaining
      (this.copy(developmentRemaining = 0), remainingPoints)
    }
    else{
      (this.copy(developmentRemaining = this.developmentRemaining - points), 0)
    }
  }

  def applyTestingAndReturnRemainder(points: Int): (StoryCard, Int) = {
    var remainingPoints: Int = 0
    if(points > testingRemaining){
      remainingPoints = points - testingRemaining
      (this.copy(testingRemaining = 0), remainingPoints)
    }
    else{
      (this.copy(testingRemaining = this.testingRemaining - points), 0)
    }
  }

  def candidateState: Phase = this match {
    case StoryCard(0, 0, 0, _) => ReadyToDeploy
    case StoryCard(0, 0, _, _) => Test
    case StoryCard(0, _, _, _) => DevInProgress
    case _ => AnalysisInProgress
  }

  def advance(board: Board): StoryCard = this match {
    case c if c.candidateState == AnalysisInProgress && board.allowAnalysisWork => this.copy(state = AnalysisInProgress)
    case c if c.candidateState == DevInProgress && c.state < DevInProgress && !board.allowDevelopmentWork => this.copy(state = AnalysisDone)
    case c if c.candidateState == DevInProgress && board.allowDevelopmentWork => this.copy(state = DevInProgress)
    case c if c.candidateState == Test && c.state < Test && !board.allowTestingWork => this.copy(state = DevDone)
    case c if c.candidateState == Test => this.copy(state = Test)
    case c if c.candidateState == ReadyToDeploy => this.copy(state = ReadyToDeploy)
    case _ => this
  }
}
