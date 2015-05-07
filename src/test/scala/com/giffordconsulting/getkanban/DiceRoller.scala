package com.giffordconsulting.getkanban

import scala.util.Random

abstract class  DiceRoller{
  def roll(): Integer
}
object DiceRoller {

  object allThrees extends DiceRoller(){
    def roll():Integer = {3}
  }

  object random extends DiceRoller{
    def roll():Integer = {Random.nextInt(5)+1}
  }

}
