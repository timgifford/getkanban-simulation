package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.BusinessValue._
import com.giffordconsulting.getkanban.Phase._

case class StoryCard(val analysisRemaining: Int, val developmentRemaining: Int, val testingRemaining: Int, val businessValue: BusinessValue = BusinessValue.Med, val actualSubscribers: Int = 0, val state: Phase = Ready) {
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
    case StoryCard(0, 0, 0, _, _, _) => ReadyToDeploy
    case StoryCard(0, 0, _, _, _, _) => Test
    case StoryCard(0, _, _, _, _, _) => DevInProgress
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

object StoryCard {
  val s1 = StoryCard(0, 0, 0, Low, 5)
  val s2 = StoryCard(0, 0, 2, Low, 6)
  val s3 = StoryCard(0, 0, 9, Med, 10)
  val s4 = StoryCard(0, 0, 4, High, 12)
  val s5 = StoryCard(0, 0, 6, Med, 11)
  val s6 = StoryCard(0, 7, 8, High, 14)
  val s7 = StoryCard(0, 9, 10, High, 13)
  val s8 = StoryCard(0, 5, 9, Med, 11)
  val s9 = StoryCard(0, 6, 10, Low, 14)
  val s10 = StoryCard(6, 5, 11, High, 12)
  val s11 = StoryCard(3, 4, 9, Low, 8)
  val s12 = StoryCard(7, 8, 11, High, 12)
  val s13 = StoryCard(3, 5, 10, Med, 9)
  val s14 = StoryCard(5, 6, 9, Med, 11)
  val s15 = StoryCard(3, 5, 6, Low, 5)
  val s16 = StoryCard(4, 5, 8, Low, 8)
  val s17 = StoryCard(6, 8, 8, High, 6)
  val s18 = StoryCard(5, 6, 9, Low, 8)
  val s19 = StoryCard(4,5,7, Low, 8)
  val s20 = StoryCard(3,5,0, High, 7)
  val s21 = StoryCard(7,8,10, Low, 14)
  val s22 = StoryCard(4,4,7, High, 8)
  val s23 = StoryCard(5,7,9, Med, 9)
  val s24 = StoryCard(5,6,6, Med, 11)
}
