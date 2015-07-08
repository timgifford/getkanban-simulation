package com.giffordconsulting.getkanban

import com.giffordconsulting.getkanban.StoryCard._

class FirstInFirstOutTest extends UnitSpec {

  "Sort" should "not reorder" in {
    val list = Seq(s1, s2, s3, s4).sorted(new FirstInFirstOut)
    list should contain.inOrder(s1,s2,s3,s4)
  }
}
