package com.giffordconsulting.getkanban

case class Points(val analysis:Int, val development: Int, val testing: Int) {

}
object Points {
  def empty = new Points(0,0,0)
}
