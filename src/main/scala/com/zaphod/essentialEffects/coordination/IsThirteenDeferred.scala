package com.zaphod.essentialEffects.coordination

import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref}
import cats.implicits.catsSyntaxTuple3Parallel
import com.zaphod.util.Debug.DebugHelper

import scala.concurrent.duration.DurationInt

object IsThirteenDeferred extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      ticks <- Ref[IO].of(0)
      is13  <- Deferred[IO, Long]
      _     <- (beepWhen13(is13), tickingClock(ticks, is13), printTicks(ticks)).parTupled
    } yield ExitCode.Success

  def beepWhen13(is13: Deferred[IO, Long]): IO[Unit] =
    for {
      _ <- IO("START listening...").debug
      t <- is13.get
      _ <- IO(s"BEEP! at $t").debug
    } yield ()

  def tickingClock(ticks: Ref[IO, Int], is13: Deferred[IO, Long]): IO[Unit] =
    for {
      _     <- IO.sleep(1.second)
      ts    =  System.currentTimeMillis()
      _     <- IO(ts).debug
      count <- ticks.updateAndGet(_ + 1)
      _     <- if (count >= 13) is13.complete(ts) else IO.unit
      _     <- if (count <= 20) tickingClock(ticks, is13) else IO(s"tickingClock stops at $ts...").debug
    } yield ()

  def printTicks(ticks: Ref[IO, Int]): IO[Unit] =
    for {
      _ <- IO.sleep(5.seconds)
      n <- ticks.get
      _ <- IO(s"TICKS: $n").debug
      _ <- if (n <= 20) printTicks(ticks) else IO("Stopping printTicks...").debug
    } yield ()
}
