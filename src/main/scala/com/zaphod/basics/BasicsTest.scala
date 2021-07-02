package com.zaphod.basics

import cats.implicits._

object BasicsTest {
  import cats.Show

  import cats.instances.int._
  import cats.instances.string._

  val showInt: Show[Int]       = Show.apply[Int]
  val showString: Show[String] = Show.apply[String]

  val s1: String = showInt.show(42)
  val s2: String = showString.show("foo")

  import cats.syntax.show._

  val s3: String = 42.show
  val s4: String = "foo".show
}

object Hierarchie {
  import scala.language.higherKinds

  trait Functor[F[_], A] {
    def map[B](fa: F[A])(f: A => B): F[B]
  }

  trait Applicative[F[_], A] extends Functor[F, A] {
    def pure(a: => A): F[A]
    def apply[B](ff: F[A => B])(fa: F[B]): F[B]
  }

  trait Monad[F[_], A] extends Applicative[F, A] {
    def flatMap[B](fa: F[A])(f: A => F[B]): F[B]
  }

  trait Semigroup[A] {
    def combine(x: A, y: A): A
  }

  trait Monoid[A] extends Semigroup[A] {
    def empty: A
  }
}


object Talk {

import cats.Monoid
import cats.instances.int._ // for Monoid

Monoid[Int].combine(32, 10)
// res5: Int = 42

import cats.Monoid
import cats.instances.int._    // for Monoid
import cats.instances.option._ // for Monoid

val a = Option(22)
// a: Option[Int] = Some(22)
val b = Option(20)
// b: Option[Int] = Some(20)

Monoid[Option[Int]].combine(a, b)
// res6: Option[Int] = Some(42)

  import cats.syntax.semigroup._

  a |+| b

//  val l1 = List(1, 2, 3).map(Option(_)).filter(item => item == 1)

import cats.Eq
import cats.instances.int._

val eqInt = Eq[Int]
import cats.syntax.eq._ // for === and =!=”

// val l2 = List(1, 2, 3).map(Option(_)).filter(item => item === 1)

}

object Laws {
  import cats.Monoid

  def associativeLaw[A](x: A, y: A, z: A)
                       (implicit m: Monoid[A]): Boolean = {
    m.combine(x, m.combine(y, z)) == m.combine(m.combine(x, y), z)
  }

  def identityLaw[A](x: A)
                    (implicit m: Monoid[A]): Boolean = {
    (m.combine(x, m.empty) == x) && (m.combine(m.empty, x) == x)
  }
}

object Function1Functor {
  import cats.instances.function._ // for Functor
  import cats.syntax.functor._     // for map

  val func1: Int => Double =
    (x: Int) => x.toDouble

  val func2: Double => Double =
    (y: Double) => y * 2

  (func1 map func2)(1)     // composition using map
  // res7: Double = 2.0

  (func1 andThen func2)(1) // composition using andThen
  // res8: Double = 2.0

  func2(func1(1))          // composition written out by hand
  // res9: Double = 2.0
}

object DoMath {
  import cats.Functor
  import cats.syntax.functor

  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  import cats.instances.option._ // for Functor
  import cats.instances.list._   // for Functor

  doMath(Option(20))
  doMath(List(1, 2, 3))
}

object SemiAppl {
  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal

  Semigroupal[Option].product(Some(123), Some("abc"))
  // res0: Option[(Int, String)] = Some((123,abc))

  Semigroupal[Option].product(None, Some("abc"))
  // res1: Option[(Nothing, String)] = None

  import cats.instances.list._ // for Semigroupal

  Semigroupal[List].product(List(1, 2), List(3, 4))
  // res5: List[(Int, Int)] = List((1,3), (1,4), (2,3), (2,4))
}

object Validate {

  import cats.Semigroupal
  import cats.data.Validated
  import cats.instances.list._ // for Monoid

  type AllErrorsOr[A] = Validated[List[String], A]

  Semigroupal[AllErrorsOr].product(
    Validated.invalid(List("Error 1")),
    Validated.invalid(List("Error 2"))
  )
  // res1: AllErrorsOr[(Nothing, Nothing)] = Invalid(List(Error 1, Error 2))
}

object ApplNeed extends App {
  val o1 = Option(1)
  val o2 = Option(2)

  val f: (Int, Int) => Int = (x, y) => x + y

  val ff = o1.map(i => f(i, _: Int)) // Option[Int => Int]

  import cats.Apply

  val o3 = Apply[Option].ap(ff)(o2)

  println(s"f($o1, $o2) = $o3")
}

