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

    import cats.instances.future._
    import scala.concurrent.ExecutionContext.Implicits.global

    val futureEitherOption: FutureEitherOption[Int] = for {
      a <- 10.pure[FutureEitherOption]
      b <- 32.pure[FutureEitherOption]
    } yield a + b

    import cats.instances.option._

    // using kind projector
    val doom = 123.pure[EitherT[Option, String, ?]]

    val errorStack1 = OptionT[ErrorOr, Int](Right(Some(10)))
    val errorStack2 = 32.pure[ErrorOrOption]

    val unpacked1 = errorStack1.value
    val unpacked2 = errorStack2.value.map(_.getOrElse(-1))

    val f1 = futureEitherOption.value
    val f2 = f1.value
  }

  import Basics._

  import scala.concurrent.Await
  import scala.concurrent.duration._

  val r1 = Await.result(f2, 1.second)

  println(s"r1: $r1")

  object UsagePattern {
    import cats.data.Writer

    type Logged[A] = Writer[List[String], A]

    // Methods generally return untransformed stack:
    def parseNumber(str: String): Logged[Option[Int]] =
      util.Try(str.toInt).toOption match {
        case Some(num) => Writer(List(s"Read '$str'"), Some(num))
        case None      => Writer(List(s"Failed on '$str'"), None)
      }

    // Comsumers use monad transformers to locally simplify composition
    def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {
      import cats.data.OptionT
      import cats.instances.list._

      val result = for {
        a <- OptionT(parseNumber(a))
        b <- OptionT(parseNumber(b))
        c <- OptionT(parseNumber(c))
      } yield a + b + c

      result.value
    }

    val r1 = addAll("1", "2", "3")
    val r2 = addAll("1", "a", "3")

    println(s"r1 = $r1")
    println(s"r2 = $r2")
  }

  object Exercise {
    import cats.data.EitherT
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    type Response[A] = EitherT[Future, String, A] // Future[Either[String, A]]

    private val powerLevels = Map(
      "Jazz"      -> 6,
      "Bumblebee" -> 8,
      "Hot Rod"   -> 10
    )

    private def getPowerLevel(autobot: String): Response[Int] = {
      import cats.instances.future._

      val bot = powerLevels.get(autobot)

      bot.fold(EitherT.left[Int](Future(s"$autobot unreachable"))) { bot => EitherT.right[String](Future(bot)) }
    }

    private def canSpecialMove(bot1: String, bot2: String): Response[Boolean] = {
      import cats.instances.future._

      for {
        l1 <- getPowerLevel(bot1)
        l2 <- getPowerLevel(bot2)
      } yield (l1 + l2) > 15
    }

    def tacticalReport(bot1: String, bot2: String): String = {
      val stack = canSpecialMove(bot1, bot2).value

      Await.result(stack, 1.second) match {
        case Left(msg)    => s"Error $msg"
        case Right(true)  => s"$bot1 and $bot2 can move"
        case Right(false) => s"$bot1 and $bot2 not enough power"
      }
    }
  }

  UsagePattern

  import Exercise._

  val report1 = tacticalReport("Jazz", "Bumblebee")
  val report2 = tacticalReport("Bumblebee", "Hot Rod")
  val report3 = tacticalReport("Jazz", "Joss")

  println(s"report1 = $report1")
  println(s"report2 = $report2")
  println(s"report3 = $report3")
}
