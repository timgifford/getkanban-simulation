package com.giffordconsulting.getkanban

class Kanban(var items: Seq[WorkItem] = Seq()) {
  
  def addToReady(s: WorkItem): Unit = {
    items = items :+ s
  }

}
