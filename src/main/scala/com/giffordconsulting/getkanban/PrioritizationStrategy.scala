package com.giffordconsulting.getkanban

class BusinessValueStrategy extends Ordering[StoryCard] {

  override def compare(x: StoryCard, y: StoryCard): Int = {
    y.businessValue.compare(x.businessValue)
  }
}

class LowestRemainingEffortFirst extends Ordering[StoryCard] {
  override def compare(x: StoryCard, y: StoryCard): Int = {
    x.remainingWork().compare(y.remainingWork())
  }
}

class FirstInFirstOut extends Ordering[StoryCard]{
  override def compare(x: StoryCard, y: StoryCard): Int = {
    0
  }
}

class BusinessValueDividedByDuration extends Ordering[StoryCard]{
  override def compare(x: StoryCard, y: StoryCard): Int = {
    x.costOfDelayDividiedByDuration().compare(y.costOfDelayDividiedByDuration())
  }
}

// TODO: sort based on the work remaining for the current queue.
class LowestRemainingWorkForCurrentQueue extends Ordering[StoryCard]{
  override def compare(x: StoryCard, y: StoryCard): Int = {
    if(x.state == y.state){
      return x.state match {
        case  Phase.AnalysisInProgress => x.analysisRemaining.compare(y.analysisRemaining)
        case _ => 0
      }
    }
    0
  }
}