package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.BusinessValue._
import com.giffordconsulting.getkanban.Phase._
import com.giffordconsulting.getkanban.StoryCard._

class LowestRemainingEffortFirstSpec extends UnitSpec {

  "LowestRemainingWork" should "sort using current phase" in {
    val list = Seq(
    StoryCard(0,0,2,state = Test, name = "test2"),
    StoryCard(0,0,1,state = Test, name = "test1"),
    StoryCard(0,0,20,state = DevDone, name = "dev2"),
    StoryCard(0,0,10,state = DevDone, name = "dev1")
    )

    val sorted: Seq[StoryCard] = list.sorted(new LowestRemainingEffortFirst)
    sorted.groupBy(x => x.state).get(Test).get(0).name should be ("test1")

  }

  "Strategy" should "sort lowest remaining effort first" in {
    val lowTestEffort = StoryCard(0, 0, 2, Low, 6, Test)
    val highTestEffort = StoryCard(0, 0, 9, Med, 10, Test)
    val highDevEffort = StoryCard(0,100,5,High, 100, DevInProgress)
    val medDevEffort = StoryCard(0,5,5,High, 100, DevInProgress)
    val cards: Seq[StoryCard] = Seq(highTestEffort, lowTestEffort, highDevEffort, medDevEffort)
    val board = Board(cards, strategy = new LowestRemainingEffortFirst())

    board.cardsInPhase(DevInProgress).head should be (medDevEffort)
    board.cardsInPhase(DevInProgress).last should be (highDevEffort)

    board.cardsInPhase(Test).head should be(lowTestEffort)
    board.cardsInPhase(Test).last should be(highTestEffort)
  }

}

class BusinessValueStrategySpec extends UnitSpec {

  "Strategy" should "sort cards using business value" in {
    val low = StoryCard(0, 0, 2, Low, 6, Test)
    val med = StoryCard(0, 0, 9, Med, 10, Test)
    val highDev = StoryCard(0,5,5,High, 100, DevInProgress)
    val cards: Seq[StoryCard] = Seq(low, highDev, med)
    val board = Board(cards, strategy = new BusinessValueStrategy())

    board.cardsInPhase(DevInProgress).head should be (highDev)

    board.cardsInPhase(Test).head should be(med)
    board.cardsInPhase(Test).last should be(low)
  }
}





class BoardSpec extends UnitSpec {

  val cardInTest: StoryCard = StoryCard(0, 0, 1, state = Test)
  val readyToDeployCard: StoryCard = StoryCard(0, 0, 0, state = ReadyToDeploy)

  "New board" should "not advance cards" in {
    val board = Board(Seq(StoryCard(0,0,0)))
    board.cards.head.state should be (Ready)
  }



  it should "start on day 9" in {
    val board = Board(Seq(StoryCard(0,0,0)))
    board.day should be (WorkDay.Nine)
  }

  "playTesting" should "advance tested cards to ReadyForDeployment" in {
    val initialCards: Seq[StoryCard] = Seq(StoryCard(0, 0, 2, state=Test), cardInTest)
    val boardWithTestingWorkApplied: Board = Board(initialCards)
      .playTesting(2)

    boardWithTestingWorkApplied.cards(0).state should be (ReadyToDeploy)
    boardWithTestingWorkApplied.cards(1).state should be (Test)
  }

  it should "apply points using the prioritization strategy" in {
    val initialCards: Seq[StoryCard] = Seq(StoryCard(0, 0, 1, state=Test, businessValue = Low, name="low"), StoryCard(0, 0, 1, state=Test, businessValue = High, name="high"))
    val boardWithTestingWorkApplied: Board = Board(initialCards,strategy = new BusinessValueStrategy).playTesting(1)
    boardWithTestingWorkApplied.cards.head.name should be ("high")
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
    board.unplayedPoints should be(Points(5,3,5))
  }

  it should "only apply points to current test queued items" in {
    val board: Board = Board(Seq(StoryCard(0,0,2, state=Test)), day=WorkDay.Ten).play(Points(5,5,5))

    board.day should be (WorkDay.Eleven)
    board.cards.head.state should be (ReadyToDeploy)
  }

  it should "puts board to day 10 after first day" in {
    val board = Board(Seq(readyToDeployCard)).play(Points(5,5,5))
    board.day should be (WorkDay.Ten)
  }

  it should "deploy items on day 9" in {
    val board = Board(Seq(readyToDeployCard), day=WorkDay.Nine).play(Points(5,5,5))
    board.cards.head.state should be (Deployed)
  }

  it should "deploy items on day 12" in {
    val board = Board(Seq(readyToDeployCard), day=WorkDay.Twelve).play(Points(5,5,5))
    board.cards.head.state should be (Deployed)
  }

  it should "deploy items on day 15" in {
    val board = Board(Seq(readyToDeployCard), day=WorkDay.Fifteen).play(Points(5,5,5))
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

    board.cards.last.state should be (Ready)
//    board.cards.tail.foreach(c => c.state should be (AnalysisInProgress))
  }

  it should "maintain at AnalysisDone state when Development at wip limit" in {
    val cardInDevelopment: StoryCard = StoryCard(0, 5, 5, state = DevInProgress)
    val fillDevelopment: Seq[StoryCard] = Seq(cardInDevelopment, cardInDevelopment, cardInDevelopment, cardInDevelopment)
    val board: Board = Board(fillDevelopment).add(StoryCard(0, 5, 5))

    board.cards.last.state should be(AnalysisDone)
    board.cards.init.foreach(c => c.state should be (DevInProgress))
  }

  it should "move second item to AnaysisInProgress" in {
    val board: Board = Board().add(StoryCard(3, 4, 9)).add(StoryCard(3, 4, 9))
    board.cards.foreach(c => c.state should be(AnalysisInProgress))
  }


  it should "maintain at DevelopmentDone when Testing at wip limit" in {
    val cardInTesting: StoryCard = cardInTest
    val fillTests: Seq[StoryCard] = Seq(cardInTesting, cardInTesting, cardInTesting)
    val board: Board = Board(fillTests).add(StoryCard(0, 0, 1))

    board.cards.last.state should be (DevDone)
    board.cards.init.foreach(c => c.state should be (Test))
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

  it should "puts last item on tail" in {
    var board = Board()
      .add(StoryCard(0, 0, 1)) // Head

    board.cards.head.analysisRemaining should be(0)
    board.cards.head.state should be (Test)

    board = board.add(StoryCard(1, 1, 1))
    board.cards.length should be (2)
    board.cards.last.analysisRemaining should be(1)
    board.cards.last.state should be (AnalysisInProgress)
  }

  it should "initiialize the board" in {
    val board = Board.startingLayout(new FirstInFirstOut())

    board.cards.length should be(12)

    board.cards(0).state should be(ReadyToDeploy)
    board.cards(1).state should be(Test)
    board.cards(2).state should be(Test)
    board.cards(3).state should be(Test)
    board.cards(4).state should be(DevDone)
    board.cards(5).state should be(DevInProgress)
    board.cards(6).state should be(DevInProgress)
    board.cards(7).state should be(DevInProgress)
    board.cards(8).state should be(AnalysisDone)
    board.cards(9).state should be(AnalysisInProgress)
    board.cards(10).state should be(Ready)
    board.cards(11).state should be(Ready)
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





