package com.giffordconsulting.getkanban

class WorkRemaining(var analysis: Int, var development: Int, var test: Int) {
}

object WorkRemaining {
  case object none extends WorkRemaining(0,0,0)
}
