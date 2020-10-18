package com.zaphod.stuff

object MethodEnhancementTest extends App {

  implicit class OptionOpsInt(i: Int) {
    def some: Option[Int] = Option[Int](i)
    def none: Option[Int]  = Option.empty[Int]  // anyways this makes no sense
  }

  val o1 = 1.some
  val n1 = 1.none

  println(s"o1: $o1")
  println(s"n1: $n1")
}
