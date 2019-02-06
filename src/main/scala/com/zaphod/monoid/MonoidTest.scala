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

//  implicit def tupleMonoid[A: Monoid, B: Monoid]: Monoid[(A, B)] = {
//    new Monoid[(A, B)] {
//      def combine(x: (A, B), y: (A, B)): (A, B) = {
//        val (xa, xb) = x
//        val (ya, yb) = y
//        (Monoid[A].combine(xa, ya), Monoid[B].combine(xb, yb))
//      }
//      def empty: (A, B) = (Monoid[A].empty, Monoid[B].empty)
//    }
//  }

  val lc = l.foldMap(i => (i, i.toString))

  println(s"l  = $l")
  println(s"li = $li")
  println(s"ls = $ls")
  println(s"lc = $lc")
}

object BoxMonoidTest extends App {
  import cats.instances.int._
  import cats.instances.string._

  case class Box[T](t: T)

  val bi1 = Box(1)
  val bi2 = Box(2)

  val bs1 = Box("s1")
  val bs2 = Box("s2")

  object WithMonoid {
    import cats.Monoid
    import cats.syntax.semigroup._

    implicit def boxMonoid[T: Monoid]: Monoid[Box[T]] = new Monoid[Box[T]] {
      override def empty: Box[T] = Box(Monoid[T].empty)

      override def combine(x: Box[T], y: Box[T]): Box[T] = Box(Monoid[T].combine(x.t, y.t))
    }

    val bi3 = bi1 |+| bi2
    val bi4 = List(bi1, bi2, bi3).combineAll

    println(s"$bi1 + $bi2 = $bi3 | $bi4")

    val bs3 = bs1 |+| bs2
    val bs4 = List(bs1, bs2, bs3).combineAll

    println(s"$bs1 + $bs2 = $bs3 | $bs4")
  }

  object WithApplicative {
    import cats.Applicative

    implicit val boxApplicative = new Applicative[Box] {
      override def pure[A](a: A): Box[A] = Box(a)

      override def ap[A, B](ff: Box[A => B])(fa: Box[A]): Box[B] = {
        val a = fa.t
        val f = ff.t

        Box(f(a))
      }
    }

    val bi5 = Applicative[Box].map2(bi1, bi2)(Monoid[Int].combine)
    val bs5 = Applicative[Box].map2(bs1, bs2)(Monoid[String].combine)

    println(s"$bi5, $bs5")
  }

  WithMonoid
  WithApplicative
}
