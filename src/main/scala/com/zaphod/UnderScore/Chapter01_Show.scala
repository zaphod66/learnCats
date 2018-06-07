package com.zaphod.UnderScore

object Chapter01_Show extends App {
  import cats.Show
  import cats.instances.int._
  import cats.instances.string._
  import cats.syntax.show._

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow = Show.show[Cat] { cat =>
    val n = cat.name.show
    val a = cat.age.show
    val c = cat.color.show

    s"$n is a $a year-old $c cat"
  }

  println(Cat("Garfield", 38, "ginger and black").show)
}
