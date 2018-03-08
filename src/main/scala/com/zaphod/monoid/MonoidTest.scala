package com.zaphod.monoid

import cats._
import cats.implicits._

object MonoidTest extends App {
  val me = Monoid[String].empty
  val cs = Monoid[String].combineAll(List("s", "a", "f", "e"))

  println(s"me: <$me>")
  println(s"cs: <$cs>")

  val l = List(1, 2, 3, 4, 5)
  val li = l.foldMap(identity)
  val ls = l.foldMap(i => i.toString)

  implicit def tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] = {
    new Monoid[(A, B)] {
      def combine(x: (A, B), y: (A, B)): (A, B) = {
        val (xa, xb) = x
        val (ya, yb) = y
        (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
      }
      def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
    }
  }

//  val lc = l.foldMap(i => (i, i.toString))

  println(s"l  = $l")
  println(s"li = $li")
  println(s"ls = $ls")
}
