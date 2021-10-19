package com.zaphod.typeerasure

object TypeErasureTest extends App {

  case class Thing[T](value: T)

  def doThing(thing: Thing[_]) = {
    thing match {
      case Thing(_: Int)    => "Thing of Int"
      case Thing(_: String) => "Thing of String"
      case Thing(_: Seq[Int]) => "Thing of Seq[Int]"
      case _                => "Thing of something"
    }
  }

  println(doThing(Thing(1)))
  println(doThing(Thing("s")))
  println(doThing(Thing(Seq(1, 2))))
  println(doThing(Thing('c')))
}
