package com.zaphod.UnderScore

import scala.language.higherKinds

object Chapter04_Monad extends App {
  object Definition {
    trait Monad[F[_]] {
      def pure[A](a: => A): F[A]

      def flapMap[A, B](fa: F[A])(f: A => F[B]): F[B]

      def map[A, B](fa: F[A])(f: A => B): F[B] = flapMap(fa)(a => pure(f(a)))
    }
  }

  object Instances {
    import cats.Monad

    import cats.instances.option._

    val opt1 = Monad[Option].pure(3)
    val opt2 = Monad[Option].flatMap(opt1)(a => Some(a + 2))
    val opt3 = Monad[Option].map(opt2)(a => 100 * a)

    import cats.instances.list._

    val lis1 = Monad[List].pure(3)
    val lis2 = Monad[List].flatMap(List(1, 2, 3))(a => List(a, a * 10))
    val lis3 = Monad[List].map(lis2)(a => a + 123)

    import cats.instances.vector._

    val vec1 = Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a * 10))

    import cats.instances.future._
    import scala.concurrent._
    import scala.concurrent.duration._
    import scala.concurrent.ExecutionContext.Implicits.global

    val mf   = Monad[Future]
    val fut1 = mf.flatMap(mf.pure(1))(x => mf.pure(x + 2))
    val res1 = Await.result(fut1, 1.second)
  }

  object Syntax {
    import cats.syntax.flatMap._
    import cats.syntax.functor._
    import cats.syntax.applicative._

    import cats.instances.option._
    import cats.instances.list._

    val opt1 = 1.pure[Option]
    val lis1 = 1.pure[List]

    import cats.Monad

    def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(y => x*x + y*y))

    val sum1 = sumSquare(3.pure[Option], 4.pure[Option])
    val sum2 = sumSquare(List(1,2,3), List(4, 5))

    println(s"sum1: $sum1")
    println(s"sum2: $sum2")
  }

  object Identity {
    import cats.Monad
    import Chapter04_Monad.Syntax.sumSquare

    type Id[A] = A

    implicit val idMonad = new Monad[Id] {
      override def pure[A](x: A): Id[A] = x
      override def flatMap[A, B](fa: Id[A])(f: A => Id[B]): Id[B] = f(fa)
      override def map[A, B](fa: Id[A])(f: A => B): Id[B] = f(fa)

      override def tailRecM[A, B](a: A)(f: A => Id[Either[A, B]]): Id[B] = ???
    }

    val sum1 = sumSquare(2: Id[Int], 3: Id[Int])

    println(s"sum1: $sum1")
  }

  Syntax
  Identity
}
