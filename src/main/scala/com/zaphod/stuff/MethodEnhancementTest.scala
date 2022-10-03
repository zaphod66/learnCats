package com.zaphod.stuff

object MethodEnhancementTest extends App {

//  implicit class OptionOpsInt(i: Int) {
//    def some: Option[Int] = Option[Int](i)
//    def none: Option[Int] = Option.empty[Int]  // anyways this makes no sense
//  }

  implicit class OptionOps[A](a: A) {
    def some: Option[A] = Option[A](a)
    def none: Option[A] = Option.empty[A]
  }

  val o1 = 1.some
  val n1 = 1.none

  val o2 = 'a'.some
  val n2 = 'a'.none

  println(s"o1: $o1")
  println(s"n1: $n1")
  println(s"o2: $o2")
  println(s"n2: $n2")
}
