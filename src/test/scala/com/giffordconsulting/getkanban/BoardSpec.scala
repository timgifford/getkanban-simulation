package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.Phase._
import com.giffordconsulting.getkanban.StoryCard._

class BoardSpec extends UnitSpec {

  val cardInTest: StoryCard = StoryCard(0, 0, 1, state = Test)
  val readyToDeployCard: StoryCard = StoryCard(0, 0, 0, state = ReadyToDeploy)

  "New board" should "not advance cards" in {
    val board = Board(Seq(StoryCard(0,0,0)))
    board.cards.head.state should be (Ready)
  }

  it should "start on day 9" in {
    val board = Board(Seq(StoryCard(0,0,0)))
    board.day should be (WorkDay(9))
  }

  "playTesting" should "advance tested cards to ReadyForDeployment" in {
    val initialCards: Seq[StoryCard] = Seq(StoryCard(0, 0, 2, state=Test), cardInTest)
    val boardWithTestingWorkApplied: Board = Board(initialCards)
      .playTesting(2)

    boardWithTestingWorkApplied.cards(0).state should be (ReadyToDeploy)
    boardWithTestingWorkApplied.cards(1).state should be (Test)
  }
  it should "have same number of cards when more points played than are remaining" in {
    val initialCards: Seq[StoryCard] = Seq(StoryCard(0, 0, 2, state=Test), cardInTest)
    val boardWithTestingWorkApplied: Board = Board(initialCards)
      .playTesting(10)

    boardWithTestingWorkApplied.cards.size should be (initialCards.size)
  }


  "playDev" should "advance development to testing" in {
    val initialCards: Seq[StoryCard] = Seq(StoryCard(0, 2, 2, state=DevInProgress), StoryCard(0, 2, 2, state=DevInProgress))
    val boardWithDevWorkApplied = Board(initialCards)
      .playDev(4)

    boardWithDevWorkApplied.cards(0).state should be (Test)
    boardWithDevWorkApplied.cards(1).state should be (Test)
  }

  it should "have same number of cards when more points played than are remaining" in {
    val initialCards: Seq[StoryCard] = Seq(StoryCard(0, 2, 2, state=DevInProgress), StoryCard(0, 2, 2, state=DevInProgress))
    val boardWithDevWorkApplied = Board(initialCards)
      .playDev(10)

    boardWithDevWorkApplied.cards.size should be (initialCards.size)
  }

  "playAnalysis" should "advance analysis to development" in {
    val initialCards: Seq[StoryCard] = Seq(StoryCard(2, 2, 2, state=AnalysisInProgress), StoryCard(2, 2, 2, state=AnalysisInProgress))
    val boardWithWorkApplied = Board(initialCards)
      .playAnalysis(4)


    boardWithWorkApplied.cards(0).state should be (DevInProgress)
    boardWithWorkApplied.cards(1).state should be (DevInProgress)
  }

  it should "have same number of cards when more points played than are remaining" in {
    val initialCards: Seq[StoryCard] = Seq(StoryCard(2, 2, 2, state=AnalysisInProgress), StoryCard(2, 2, 2, state=AnalysisInProgress))
    val boardWithWorkApplied = Board(initialCards)
      .playAnalysis(10)

    boardWithWorkApplied.cards.size should be (initialCards.size)
  }

  "Play" should "only apply points to current analysis queued items" in {
    val board: Board = Board(Seq(StoryCard(2,2,2,state=AnalysisInProgress))).play(Points(5,5,5))

    board.cards.head.state should be (DevInProgress)
  }

  it should "only apply points to current development queued items" in {
    val board: Board = Board(Seq(StoryCard(0,2,2, state=DevInProgress))).play(Points(5,5,5))

    board.cards.head.state should be (Test)
    board.remainingPoints should be(Points(5,3,5))
  }

  it should "only apply points to current test queued items" in {
    val board: Board = Board(Seq(StoryCard(0,0,2, state=Test)), day=WorkDay(10)).play(Points(5,5,5))

    board.day should be (WorkDay(11))
    board.cards.head.state should be (ReadyToDeploy)
  }

  it should "puts board to day 10 after first day" in {
    val board = Board(Seq(readyToDeployCard)).play(Points(5,5,5))
    board.day should be (WorkDay(10))
  }

  it should "deploy items on day 9" in {
    val board = Board(Seq(readyToDeployCard), day=WorkDay(9)).play(Points(5,5,5))
    board.cards.head.state should be (Deployed)
  }

  it should "deploy items on day 12" in {
    val board = Board(Seq(readyToDeployCard), day=WorkDay(12)).play(Points(5,5,5))
    board.cards.head.state should be (Deployed)
  }

  it should "deploy items on day 15" in {
    val board = Board(Seq(readyToDeployCard), day=WorkDay(15)).play(Points(5,5,5))
    board.cards.head.state should be (Deployed)
  }

  it should "work with empty board" in {
    val board = Board().play(Points(5,5,5))
    board.cards should be(empty)
  }

  "Add" should "advance to DevInProgress for a story without analysis work" in {
    val devInProgress: StoryCard = new StoryCard(0, 5, 5)
    val board = Board().add(devInProgress)

    board.cards(0).candidateState should be(DevInProgress)
    board.cards(0).state should be(DevInProgress)
  }

  it should "not allow more than 4 in Ready" in {
    var board: Board = Board(Seq(
      StoryCard(1,0,0, state=AnalysisInProgress),
      StoryCard(1,0,0, state=AnalysisInProgress)))

    // Add 4 Ready items
    for(i <- 1 to 4) board = board.add(StoryCard(1,1,1))

    board.allowReadyWork should be(false)

    intercept[IllegalArgumentException] {
      board.add(StoryCard(1, 1, 1))
    }
  }

  it should "maintain at Ready when Analysis at wip limit" in {
    val cardInAnalysis: StoryCard = StoryCard(1, 1, 1, state=AnalysisInProgress)
    val fillAnalysis: Seq[StoryCard] = Seq(cardInAnalysis, cardInAnalysis)
    val board: Board = Board(fillAnalysis).add(StoryCard(1, 1, 1))

    board.cards.head.state should be (Ready)
    board.cards.tail.foreach(c => c.state should be (AnalysisInProgress))
  }

  it should "maintain at AnalysisDone state when Development at wip limit" in {
    val cardInDevelopment: StoryCard = StoryCard(0, 5, 5, state = DevInProgress)
    val fillDevelopment: Seq[StoryCard] = Seq(cardInDevelopment, cardInDevelopment, cardInDevelopment, cardInDevelopment)
    val board: Board = Board(fillDevelopment).add(StoryCard(0, 5, 5))

    board.cards.head.state should be(AnalysisDone)

    board.cards.tail.foreach(c => c.state should be (DevInProgress))
  }

  it should "move second item to AnaysisInProgress" in {
    val board: Board = Board().add(StoryCard(3, 4, 9)).add(StoryCard(3, 4, 9))
    board.cards.foreach(c => c.state should be(AnalysisInProgress))
  }


  it should "maintain at DevelopmentDone when Testing at wip limit" in {
    val cardInTesting: StoryCard = cardInTest
    val fillTests: Seq[StoryCard] = Seq(cardInTesting, cardInTesting, cardInTesting)
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

  it should "puts last item on head" in {
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





