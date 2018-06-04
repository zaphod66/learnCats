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

    val mf = Monad[Future]
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

    def sumSquare[F[_] : Monad](a: F[Int], b: F[Int]): F[Int] = a.flatMap(x => b.map(y => x * x + y * y))

    val sum1 = sumSquare(3.pure[Option], 4.pure[Option])
    val sum2 = sumSquare(List(1, 2, 3), List(4, 5))

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

  object EitherUse {

    import Chapter04_Monad.Syntax.sumSquare
    import cats.instances.either._

    import cats.syntax.either._

    val a = 3.asRight[String]
    val b = 4.asRight[String]

    val c = sumSquare(a, b)

    val d = Either.catchOnly[NumberFormatException]("foo".toInt)
    val e = Either.catchNonFatal(sys.error("Boom"))

    val f = Either.fromOption(Option(1), "Boom")
    val g = Either.fromOption(None, "Boom")

    val h = 0.asRight[String].ensure("Must be positive")(_ > 0)

    val i = "error".asLeft[Int].recover { case _: String => -1 }
    val j = "error".asLeft[Int].recoverWith { case _: String => 1.asRight[String] }

    val k = "foo".asLeft[Int].leftMap(_.reverse)
    val l = 6.asRight[String].bimap(_.reverse, _ * 7)
    val m = "error".asLeft[Int].bimap(_.reverse, _ * 7)

    //    println(s"a: $a")
    //    println(s"b: $b")
    //    println(s"c: $c")
    println(s"d: $d")
    println(s"e: $e")
    //    println(s"f: $f")
    //    println(s"g: $g")
    //    println(s"h: $h")
  }

  object MonadErrorUse {
    import cats.MonadError
    import cats.instances.either._

    type ErrorOr[A] = Either[String, A]
    val  monadError = MonadError[ErrorOr, String]

    val success1 = monadError.pure(42)
    val failure1 = monadError.raiseError("Boom")

    monadError.handleError(failure1) {
      case "Boom" => monadError.pure("It's ok")
      case _      => monadError.raiseError("It's not ok")
    }

//    import cats.syntax.either._

    monadError.ensure(success1)("Number too low!")(_ > 1000)

    import cats.syntax.applicative._
    import cats.syntax.applicativeError._
    import cats.syntax.monadError._

    val success2 = 42.pure[ErrorOr]
    val failure2 = "Boom".raiseError[ErrorOr, Int]

    val res = success2.ensure("Number too low!")(_ > 1000)
  }

  Syntax
  Identity
  EitherUse
}