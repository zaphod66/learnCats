package com.zaphod.essentialEffects.coordination

import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref}
import cats.implicits.catsSyntaxTuple3Parallel
import com.zaphod.util.Debug.DebugHelper

import scala.concurrent.duration.DurationInt

object UseDeferred extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      ticks <- Ref[IO].of(0)
      is13  <- Deferred[IO, Unit]
      _     <- (beepWhen13(is13), tickingClock(ticks, is13), printTicks(ticks)).parTupled
    } yield ExitCode.Success

  def beepWhen13(is13: Deferred[IO, Unit]) =
    for {
      _ <- IO("START listening...").debug
      _ <- is13.get
      _ <- IO("BEEP!").debug
    } yield ()

  def tickingClock(ticks: Ref[IO, Int], is13: Deferred[IO, Unit]): IO[Unit] =
    for {
      _     <- IO.sleep(1.second)
      _     <- IO(System.currentTimeMillis()).debug
      count <- ticks.updateAndGet(_ + 1)
      _     <- if (count >= 13) is13.complete(()) else IO.unit
      _     <- tickingClock(ticks, is13)
    } yield ()

  def printTicks(ticks: Ref[IO, Int]): IO[Unit] =
    for {
      _ <- IO.sleep(5.seconds)
      n <- ticks.get
      _ <- IO(s"TICKS: $n").debug
      _ <- printTicks(ticks)
    } yield ()
}
