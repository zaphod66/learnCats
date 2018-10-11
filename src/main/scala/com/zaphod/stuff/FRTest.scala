package com.zaphod.stuff

object FRTest extends App {
  val oo1 = Option(Option(1))
  val oo2 = Option(Option(2))

  val oo3 = for {
    o1 <- oo1
    r1 <- for {
      t <- o1
    } yield t + 1
  } yield r1

  val oo4 = for {
    o1 <- oo1
  } yield for {
    o <- o1
  } yield o + 1

  println(s"oo1 = $oo1")
  println(s"oo2 = $oo2")
  println(s"oo3 = $oo3")
  println(s"oo4 = $oo4")
}
