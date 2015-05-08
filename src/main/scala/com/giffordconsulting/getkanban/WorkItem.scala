package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.BusinessValue._
import com.giffordconsulting.getkanban.Phase._
import com.giffordconsulting.getkanban.WorkDay.Ten

import scala.collection.immutable.HashMap



class WorkItem(val id: String,
               val effort: WorkRemaining = WorkRemaining.none,
               val businessValue: BusinessValue = BusinessValue.Med,
               val subscribers: Integer = 1,
               var state:Phase = Ready
                ) {

  def needsAnalysis = effort.analysis > 0
  def needsTesting = effort.test > 0
  def needsDevelopment = effort.development > 0


  def isReadyForDeployment(): Boolean = effort.analysis == 0 && effort.development == 0 & effort.test == 0
  def isReadyForDev() = effort.analysis == 0 && effort.development > 0
  def isReadyForTesting(): Boolean = effort.analysis == 0 && effort.development == 0 & effort.test > 0
  override def toString: String = this.getClass.getSimpleName + "[" + id + "]"
}

object WorkItems {

  object Empty extends WorkItem(id = "Empty", WorkRemaining.none, BusinessValue.Med, 1)

  case object  S1 extends WorkItem("S1", WorkRemaining.none, Low, 5)
  case object  S2 extends WorkItem("S2", new WorkRemaining(0, 0, 2), Low, 6)
  case object  S3 extends WorkItem("S3", new WorkRemaining(0, 0, 9), Med,  10)
  case object  S4 extends WorkItem("S4", new WorkRemaining(0, 0, 4), High,  12)
  case object  S5 extends WorkItem("S5", new WorkRemaining(0, 0, 6), Med,  11)
  case object  S6 extends WorkItem("S6", new WorkRemaining(0, 7, 8), High,  14)
  case object  S7 extends WorkItem("S7", new WorkRemaining(0, 9, 10), High,  13)
  case object  S8 extends WorkItem("S8", new WorkRemaining(0, 5, 9), Med,  11)
  case object  S9 extends WorkItem("S9", new WorkRemaining(0, 6, 10), Low,  14)
  case object S10 extends WorkItem("S10", new WorkRemaining(6, 5, 11), High, 12)
  case object S11 extends WorkItem("S11", new WorkRemaining(3, 4, 9), Low, 8)
  case object S12 extends WorkItem("S12", new WorkRemaining(7, 8, 11), High, 12)
  case object S13 extends WorkItem("S13", new WorkRemaining(3,5,10), Med, 9)
  case object S14 extends WorkItem("S14", new WorkRemaining(5,6,9), Med, 11)
  case object S15 extends WorkItem("S15", new WorkRemaining(3,5,6), Low, 5)
  case object S16 extends WorkItem("S16", new WorkRemaining(4,5,8), Low, 8)
  case object S17 extends WorkItem("S17", new WorkRemaining(6,8,8), High, 6)
  case object S18 extends WorkItem("S18", new WorkRemaining(5,6,9), Low, 8)
  case object S19 extends WorkItem("S19", new WorkRemaining(4,5,7), Low, 8)
  case object S20 extends WorkItem("S20", new WorkRemaining(3,5,0), High, 7)
  case object S21 extends WorkItem("S21", new WorkRemaining(7,8,10), Low, 14)
  case object S22 extends WorkItem("S22", new WorkRemaining(4,4,7), High, 8)
  case object S23 extends WorkItem("S23", new WorkRemaining(5,7,9), Med, 9)
  case object S24 extends WorkItem("S24", new WorkRemaining(5,6,6), Med, 11)

  val day9 = Seq(S1, S2, S3, S4, S5, S6, S7, S8, S9, S10, S11)
}

