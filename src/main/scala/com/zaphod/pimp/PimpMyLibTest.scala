package com.zaphod.pimp

import scala.language.implicitConversions

class BlingString(val str: String) extends AnyVal {
  def bling = "*" + str + "*"
}

object PimpMyLibTest extends App {
  implicit def BlingToString(str: String): BlingString = new BlingString(str)

  // implicit value class is recommended to avoid runtime creation of wrappers
  // http://docs.scala-lang.org/overviews/core/value-classes.html
  implicit class PlusString(val str: String) extends AnyVal {
    def plus = "+" + str + "+"
  }

  val s = "Mona"
  println(s"$s.bling = ${s.bling}")
  println(s"$s.plus  = ${s.plus}")
}
