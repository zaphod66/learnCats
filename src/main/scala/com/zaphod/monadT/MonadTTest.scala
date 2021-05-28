package com.zaphod.monadT

import cats._
import cats.data._
import cats.implicits._
import cats.Apply._
import cats.syntax.apply._
import cats.Applicative._
import cats.syntax.applicative._

import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.language.higherKinds

object MonadTTest extends App {

  type Vali[A] = Validated[String, A]

  def stringToInt(x: String): Vali[Int] = {
    try {
      Validated.valid(x.toInt)
    } catch {
      case _ : NumberFormatException =>
        Validated.invalid(s"Invalid Number $x ")
    }
  }

  println(stringToInt("1"))
  println(stringToInt("a"))

  val o1: Option[Int] = Option(1)
  val o2: Option[Int] = Option(2)
  val ao = Apply[Option].map2(o1, o2)((x, y) => x + y)

  val v1: Vali[Int] = stringToInt("1")
  val v2: Vali[Int] = stringToInt("2")
  val v3: Vali[Int] = stringToInt("b")

  val av1 = Apply[Vali].map2(v1, v2)((x, y) => x + y)
  val av2 = Apply[Vali].map2(v1, v3)((x, y) => x + y)
  val av3 = Apply[Vali].map2(v3, v2)((x, y) => x + y)

  //val l1 = Apply[Validated[String, Int]].map2(stringToInt("1"), stringToInt("2"))((s1, s2) => s1)

  println(s"av1 = $av1")
  println(s"av2 = $av2")
  println(s"av3 = $av3")

  val l1 = List("1",   "2",  "3")
  val l2 = List("1",  "2b", "3c")
  val l3 = List("1a", "2b", "3c")

  println( l1.traverse(stringToInt) )
  println( l2.traverse(stringToInt) )
  println( l3.traverse(stringToInt) )

  ///////////////////////////////////
  // composing Functors and Applicatives

  val FF = Functor[List] compose Functor[List]
  val AA = Applicative[List] compose Applicative[List]

  val ll1 = List(List("1","2"), List("3","4"))
  val ll2 = List(List("1","2b"), List("3c","4"))
  val ll3 = List(List(1,2), List(3,4))

  println( ll1.map(_.map(stringToInt)) )

  println( FF.map(ll1)(stringToInt) )
  println( FF.map(ll2)(stringToInt) )

  println( AA.map2(ll3, ll3)(_ -> _))

  ///////////////////////////////////
  // Monads do not compose
  // except where Monad Transformers exist

  // Our Tools:
  // def flatMap[A, B](fa: F[A])(f: A => F[B]): F[B]
  // def flatMap[A, B](ga: G[A])(f: A => G[B]): G[B]
  //
  // Our Goal:
  // def flatMap[A, B](fga: F[G[A]])(f: A => F[G[B]]): F[G[B]]  // not possible in general

  // Goal (refined)
  // def flatMapForOption[A, B](foa: F[Option[A]])(f: A => F[Option[B]]): F[Option[B]]

  // Most monad transformers boils down to a flatMap and then pattern matching

  def flatMapForOption[A, B, F[_]: Monad](foa: F[Option[A]])(f: A => F[Option[B]]): F[Option[B]] = {
    foa.flatMap {
      case None    => Monad[F].pure(None: Option[B])
      case Some(a) => f(a)
    }
  }

  val f1 = Funcs.expensiveComputation(3)
  val r1 = flatMapForOption(f1)(Funcs.expensiveComputation)

  r1 foreach println

//  val r2 = for {
//    n <- Funcs.expensiveComputationT(3)
//    m <- Funcs.expensiveComputationT(n)
//  } yield m
//
//  r2.run foreach println
}

// for using in for comprehensions
// OptionT is a wrapper => a convienence wrapper for flatMapForOption
//case class OptionT[M[_], A](run: M[Option[A]]) {
//  def flatMap[B](f: A => OptionT[M, B])(implicit M: Monad[M]): OptionT[M, B] =
//    OptionT(run.flatMap{
//      case None    => M.pure(None: Option[B])
//      case Some(a) => f(a).run
//    })
//
//  def map[B](f: A => B)(implicit M: Functor[M]): OptionT[M, B] =
//    OptionT(run.map(_.map(f)))
//}

object Funcs {
  def factorsPresent(n: Int): List[Option[Int]] =
    (1 to n).map { i =>
      if (n % i == 0)
        Some(n / i)
      else
        None
    }.toList

  def primeFactors(n: Int): List[Int] = {
    @tailrec
    def go(c: Int, d: Int, acc: List[Int]): List[Int] = {
      if (d > c)
        acc
      else if (c % d == 0)
        go(c / d, d, d :: acc)
      else
        go(c, d + 1, acc)
    }

    go(n, 2, List.empty[Int])
  }

  def expensiveComputation(n: Int): Future[Option[Int]] =
    if (n == 1)
      Future { None }
    else if (n % 2 == 0)
      Future { Some(n / 2) }
    else
      Future { Some((n + 1) / 2) }

  def factorsPresentT(n: Int): OptionT[List, Int] =
    OptionT(
      factorsPresent(n)
    )

  def expensiveComputationT(n: Int): OptionT[Future, Int] =
    OptionT(
      expensiveComputation(n)
    )
}
