package com.zaphod.stuff

object RFTest extends App {

  def abs1(i: Int): Int = {
    val pos = return  i
    val neg = return -i

    if (i > 0) pos
    else       neg
  }

  def abs2(i: Int): Int = {
    if (i > 0) return  i
    else       return -i
  }

  println(s"foo1(1):  ${abs1(1)}")
  println(s"foo1(-1): ${abs1(-1)}")
  println(s"foo2(1):  ${abs2(1)}")
  println(s"foo2(-1): ${abs2(-1)}")
}
