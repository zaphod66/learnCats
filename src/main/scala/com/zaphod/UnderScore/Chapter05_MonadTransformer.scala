package com.zaphod.UnderScore

object Chapter05_MonadTransformer extends App {
  object Basics {
    import cats.data.OptionT
    type ListOption[A] = OptionT[List, A]   // List[Option[A]]

    type ErrorOr[A]       = Either[String, A]
    type ErrorOrOption[A] = OptionT[ErrorOr, A]

    import cats.instances.either._
    import cats.syntax.applicative._

    val a = 10.pure[ErrorOrOption]
    val b = 32.pure[ErrorOrOption]
    val c = a.flatMap(x => b.map(y => x + y))

    import scala.concurrent.Future
    import cats.data.EitherT

    type FutureEither[A]       = EitherT[Future, String, A]
    type FutureEitherOption[A] = OptionT[FutureEither, A]

    import cats.instances.future
    import scala.concurrent.Await
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration

    val futureEitherOption: FutureEitherOption[Int] = for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

  }
}
