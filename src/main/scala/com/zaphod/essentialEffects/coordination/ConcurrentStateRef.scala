package com.zaphod.essentialEffects.coordination

import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.implicits.catsSyntaxTuple2Parallel
import com.zaphod.util.Debug.DebugHelper

import scala.concurrent.duration.DurationInt

object ConcurrentStateRef extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      ticks <- Ref[IO].of(0)
      _     <- (tickingClock(ticks), printTicks(ticks)).parTupled
    } yield ExitCode.Success

  def tickingClock(ticks: Ref[IO, Int]): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- ticks.update(_ + 1)
      _ <- tickingClock(ticks)
    } yield ()

  def printTicks(ticks: Ref[IO, Int]): IO[Unit] =
    for {
      _ <- IO.sleep(5.seconds)
      n <- ticks.get
      _ <- IO(s"TICKS: $n").debug
      _ <- printTicks(ticks)
    } yield ()
}
