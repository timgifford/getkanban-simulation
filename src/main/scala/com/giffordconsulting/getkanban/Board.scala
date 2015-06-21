package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.Phase._

case class Board(val cards: Seq[StoryCard] = Seq.empty, val remainingPoints: Points = Points.empty, val day: WorkDay = WorkDay(9)) {

  def moveReadyToDeployToDeploy(): Board = {
    var processed: Seq[StoryCard] = Nil
    for (card <- cards) card match {
      case StoryCard(_, _, _, _, _, ReadyToDeploy) => {
        processed = processed :+ card.copy(state = Deployed)
      }
      case _ => processed = processed :+ card
    }
    this.copy(cards = processed)
  }

  def deployOnDeployDays(): Board = {
    val board: Board = day match {
      case WorkDay(9) => moveReadyToDeployToDeploy()
      case WorkDay(12) => moveReadyToDeployToDeploy()
      case WorkDay(15) => moveReadyToDeployToDeploy()
      case _ => this
    }
    board.copy(day = day.next())
  }

  def allowReadyWork = 4 > cards.count(c => c.state == Ready)

  def allowAnalysisWork = 2 > cards.count(c => c.state == AnalysisInProgress || c.state == AnalysisDone)

  def allowDevelopmentWork = 4 > cards.count(c => c.state == DevInProgress || c.state == DevDone)

  def allowTestingWork = 3 > cards.count(c => c.state == Test)

  def isBoardFull = cards.count(c => c.state < ReadyToDeploy) == 13

  def play(points: Points): Board = {
    playTesting(points.testing)
      .playDev(points.development)
      .playAnalysis(points.analysis)
      .deployOnDeployDays()
  }

  def playAnalysis(points: Int) = {

    var remainingPoints = points
    var processed: Seq[StoryCard] = Nil
    for (card <- cards) card match {
      case StoryCard(_, _, _, _, _, AnalysisInProgress) => {
        val ret: (StoryCard, Int) = card.applyAnalysisAndReturnRemainder(remainingPoints)
        remainingPoints = ret._2
        processed = processed :+ ret._1.advance(this)
      }
      case _ => processed = processed :+ card
    }
    this.copy(processed, this.remainingPoints.copy(analysis = remainingPoints))
  }

  def playDev(points: Int): Board = {
    var remainingPoints = points
    var processed: Seq[StoryCard] = Nil
    for (card <- cards) card match {
      case StoryCard(_, _, _, _, _, DevInProgress) => {
        val ret: (StoryCard, Int) = card.applyDevAndReturnRemainder(remainingPoints)
        remainingPoints = ret._2
        processed = processed :+ ret._1.advance(this)
      }
      case _ => processed = processed :+ card
    }
    this.copy(processed, this.remainingPoints.copy(development = remainingPoints))
  }

  def playTesting(points: Int): Board = {
    var remainingPoints = points
    var processed: Seq[StoryCard] = Nil
    for (card <- cards) card match {
      case StoryCard(_, _, _, _, _, Test) => {
        val ret: (StoryCard, Int) = card.applyTestingAndReturnRemainder(remainingPoints)
        remainingPoints = ret._2
        processed = processed :+ ret._1.advance(this)
      }
      case _ => {
        processed = processed :+ card
      }
    }
    this.copy(processed, this.remainingPoints.copy(testing = remainingPoints))
  }

  def add(card: StoryCard): Board = card match {
    case s if s.state != Ready => throw new IllegalArgumentException("Only add 'Ready' stories")
    case _ => {
      if (!allowReadyWork) throw new IllegalArgumentException("Board is full")
      val newCards: Seq[StoryCard] = card +: cards
      this.copy(newCards.map(_.advance(this)))
    }
  }
}
