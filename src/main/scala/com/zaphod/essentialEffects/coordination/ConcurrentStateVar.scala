package com.zaphod.essentialEffects.coordination

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Parallel
import com.zaphod.util.Debug.DebugHelper

import scala.concurrent.duration.DurationInt

object ConcurrentStateVar extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- (tickingClock, printTicks).parTupled
    } yield ExitCode.Success

  var ticks: Long = 0L

  val tickingClock: IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ = ticks = ticks + 1
      _ <- tickingClock
    } yield ()

  val printTicks: IO[Unit] =
    for {
      _ <- IO.sleep(5.seconds)
      _ <- IO(s"TICKS: $ticks").debug
      _ <- printTicks
    } yield ()
}
