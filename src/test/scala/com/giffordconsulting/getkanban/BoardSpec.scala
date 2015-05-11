package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.Phase._

class BoardSpec extends UnitSpec {

  "New board" should "not advance cards" in {
    val board = Board(Seq(StoryCard(0,0,0)))
    board.cards.head.state should be (Ready)
  }

  

  "Add" should "advance to DevInProgress for a story without analysis work" in {
    val devInProgress: StoryCard = new StoryCard(0, 5, 5)
    val board = Board().add(devInProgress)

    board.cards(0).candidateState should be(DevInProgress)
    board.cards(0).state should be(DevInProgress)
  }

  it should "not allow more than 4 in Ready" in {
    var board: Board = Board(Seq(StoryCard(1,0,0, AnalysisInProgress), StoryCard(1,0,0, AnalysisInProgress)))

    // Add 4 Ready items
    for(i <- 1 to 4) board = board.add(StoryCard(1,1,1))

    board.allowReadyWork should be(false)

    intercept[IllegalArgumentException] {
      board.add(StoryCard(1, 1, 1))
    }
  }

  it should "maintain at Ready when Analysis at wip limit" in {
    val fillAnalysis: Seq[StoryCard] = Seq(StoryCard(1, 1, 1, AnalysisInProgress), StoryCard(1, 1, 1, AnalysisInProgress))
    val board: Board = Board(fillAnalysis).add(StoryCard(1, 1, 1))

    board.cards.head.state should be (Ready)
    board.cards.tail.foreach(c => c.state should be (AnalysisInProgress))
  }

  it should "maintain at AnalysisDone state when Development at wip limit" in {
    val fillDevelopment: Seq[StoryCard] = Seq(StoryCard(0, 5, 5, DevInProgress), StoryCard(0, 5, 5, DevInProgress), StoryCard(0, 5, 5, DevInProgress), StoryCard(0, 5, 5, DevInProgress))
    val board: Board = Board(fillDevelopment).add(StoryCard(0, 5, 5))

    board.cards.head.state should be(AnalysisDone)

    board.cards.tail.foreach(c => c.state should be (DevInProgress))
  }

  it should "move second item to AnaysisInProgress" in {
    val board: Board = Board().add(StoryCard(3, 4, 9)).add(StoryCard(3, 4, 9))
    board.cards.foreach(c => c.state should be(AnalysisInProgress))
  }


  it should "maintain at DevelopmentDone when Testing at wip limit" in {
    val fillTests: Seq[StoryCard] = Seq(StoryCard(0, 0, 1, Test), StoryCard(0, 0, 1, Test), StoryCard(0, 0, 1, Test))
    val board: Board = Board(fillTests).add(StoryCard(0, 0, 1))

    board.cards.head.state should be (DevDone)
    board.cards.tail.foreach(c => c.state should be (Test))
  }

  it should "advance to ReadyToDeploy when no work remains" in {
    val board = Board().add(StoryCard(0, 0, 0))
    board.cards.head.state should be(ReadyToDeploy)
  }

  it should "advance to Test when only test remains" in {
    val board: Board = Board().add(StoryCard(0, 0, 2))
    board.cards.head.state should be(Test)
  }

  it should "only allow Ready stories" in {
    intercept[IllegalArgumentException] {
      Board().add(StoryCard(1, 2, 3, state = Deployed))
    }
  }

  it should "puts lastt item on head" in {
    var board = Board()
      .add(StoryCard(0, 0, 1)) // Tail

    board.cards.head.analysisRemaining should be(0)
    board.cards.head.state should be (Test)

    board = board.add(StoryCard(1, 1, 1))
    board.cards.length should be (2)
    board.cards.head.analysisRemaining should be(1)
    board.cards.head.state should be (AnalysisInProgress)
  }

  it should "initiialize the board" in {
    val s1 = StoryCard(0, 0, 0)
    val s2 = StoryCard(0, 0, 2)
    val s3 = StoryCard(0, 0, 9)
    val s4 = StoryCard(0, 0, 4)
    val s5 = StoryCard(0, 0, 6)
    val s6 = StoryCard(0, 7, 8)
    val s7 = StoryCard(0, 9, 10)
    val s8 = StoryCard(0, 5, 9)
    val s9 = StoryCard(0, 6, 10)
    val s10 = StoryCard(6, 5, 11)
    val s11 = StoryCard(3, 4, 9)
    val s12 = StoryCard(7, 8, 11)

    var board = Board()
    for (card <- Seq(s1, s2, s3, s4, s5, s6, s7, s8, s9, s10, s11, s12)) {
      board = board.add(card)
    }

    board.cards.length should be(12)

    board.cards(0).state should be(Ready)
    board.cards(1).state should be(Ready)

    board.cards(2).state should be(AnalysisInProgress)
    board.cards(3).state should be(AnalysisDone)

    board.cards(4).state should be(DevInProgress)
    board.cards(5).state should be(DevInProgress)
    board.cards(6).state should be(DevInProgress)
    board.cards(7).state should be(DevDone)

    board.cards(8).state should be(Test)
    board.cards(9).state should be(Test)
    board.cards(10).state should be(Test)

    board.cards(11).state should be(ReadyToDeploy)
  }

  "CandidateState" should "be DevInProgress when analysis work is completed" in {
    StoryCard(0, 1, 1).candidateState should be(DevInProgress)
  }

  it should "be AnalysisInPogress when analysis work is pending" in {
    StoryCard(1, 0, 0).candidateState should be(AnalysisInProgress)
  }

  it should "be Test when testing work is pending" in {
    StoryCard(0, 0, 1).candidateState should be(Test)
  }

  it should "be ReadyToDeploy when all work is completed" in {
    StoryCard(0, 0, 0).candidateState should be(ReadyToDeploy)
  }
}

case class StoryCard(val analysisRemaining: Int, val developmentRemaining: Int, val testingRemaining: Int, val state: Phase = Ready) {
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

case class Board(val cards: Seq[StoryCard] = Seq.empty) {
  def allowReadyWork = 4 > cards.count(c => c.state == Ready)
  def allowAnalysisWork = 2 > cards.count(c => c.state == AnalysisInProgress || c.state == AnalysisDone)
  def allowDevelopmentWork = 4 > cards.count(c => c.state == DevInProgress || c.state == DevDone)
  def allowTestingWork = 3 > cards.count(c => c.state == Test)
  def isBoardFull = {cards.count(c => c.state < ReadyToDeploy) == 13}

  def add(card: StoryCard): Board = card match {
    case s if s.state != Ready => throw new IllegalArgumentException("Only add 'Ready' stories")
    case _ => {
      if(!allowReadyWork) throw new IllegalArgumentException("Board is full")
      val newCards: Seq[StoryCard] = card +: cards
      Board(newCards.map(_.advance(this)))
    }
  }
}

