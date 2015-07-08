package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.Phase._
import com.giffordconsulting.getkanban.StoryCard._

object Board {
  def startingLayout(strategy: Ordering[StoryCard]): Board = {
    var board = Board(strategy = strategy)
    for (card <- Seq(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)) {
      board = board.add(card)
    }
    board
  }

}


case class Board(val cards: Seq[StoryCard] = Seq.empty, val unplayedPoints: Points = Points.empty, val day: WorkDay = WorkDay.Nine, val strategy: Ordering[StoryCard] = new BusinessValueStrategy()) {

  def revenue = {
    cardsInPhase(Deployed).foldLeft(0)( _ + _.revenue)
  }

  def cardsInPhase(phase: Phase) = {
    cards.groupBy(x => x.state).get(phase).getOrElse(Seq.empty).sorted(strategy)
  }

  def moveReadyToDeployToDeploy(): Board = {
    var processed: Seq[StoryCard] = Nil

    for (card <- cards) card match {
      case StoryCard(_, _, _, _, _, ReadyToDeploy, _, 0) => {
        processed = processed :+ card.copy(state = Deployed, revenue = card.actualSubscribers * day.multiplier)
      }
      case _ => processed = processed :+ card
    }
    this.copy(cards = processed)
  }

  def deployOnDeployDays(): Board = {
    var board: Board = this
    if(day.isDeployDay) {board = moveReadyToDeployToDeploy()}
    board.copy(day = day.next)
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

    var unplayedPoints = points
    var processed: Seq[StoryCard] = Nil
    for (card <- cards) card match {
      case StoryCard(_, _, _, _, _, AnalysisInProgress, _, _) => {
        val ret: (StoryCard, Int) = card.applyAnalysisAndReturnRemainder(unplayedPoints)
        unplayedPoints = ret._2
        processed = processed :+ ret._1.advance(this)
      }
      case StoryCard(_,_,_,_,_,AnalysisDone, _, _) => {
        processed = processed :+ card.advance(this)
      }
      case StoryCard(_,_,_,_,_,Ready, _, _) => {
        processed = processed :+ card.advance(this)
      }
      case _ => processed = processed :+ card
    }
    this.copy(processed.sorted(strategy), this.unplayedPoints.copy(analysis = unplayedPoints))
  }

  def playDev(points: Int): Board = {
    var unplayedPoints = points
    var processed: Seq[StoryCard] = Nil
    for (card <- cards) card match {
      case StoryCard(_, _, _, _, _, DevInProgress, _,_ ) => {
        val ret: (StoryCard, Int) = card.applyDevAndReturnRemainder(unplayedPoints)
        unplayedPoints = ret._2
        processed = processed :+ ret._1.advance(this)
      }
      case StoryCard(_, _, _, _, _, DevDone, _, _) => processed = processed :+ card.advance(this)
      case _ => processed = processed :+ card
    }
    this.copy(processed.sorted(strategy), this.unplayedPoints.copy(development = unplayedPoints))
  }

  def playTesting(points: Int): Board = {
    var unplayedPoints = points
    var processed: Seq[StoryCard] = Nil
    for (card <- cards) card match {
      case StoryCard(_, _, _, _, _, Test, _,_) => {
        val ret: (StoryCard, Int) = card.applyTestingAndReturnRemainder(unplayedPoints)
        unplayedPoints = ret._2
        processed = processed :+ ret._1.advance(this)
      }
      case _ => {
        processed = processed :+ card
      }
    }
    this.copy(processed.sorted(strategy), this.unplayedPoints.copy(testing = unplayedPoints))
  }

  def add(card: StoryCard): Board = card match {
    case s if s.state != Ready => throw new IllegalArgumentException("Only add 'Ready' stories")
    case _ => {
      if (!allowReadyWork) throw new IllegalArgumentException("Board is full")
      val newCards: Seq[StoryCard] = cards :+ card
      this.copy(newCards.sorted(strategy).map(_.advance(this)))
    }
  }
}
