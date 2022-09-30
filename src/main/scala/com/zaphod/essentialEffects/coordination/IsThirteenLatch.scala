package com.zaphod.essentialEffects.coordination

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Parallel
import com.zaphod.util.Debug.DebugHelper

import scala.concurrent.duration.DurationInt

object IsThirteenLatch extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      latch <- CountDownLatch(13)
      _     <- (beeper(latch), tickingClock(latch)).parTupled
    } yield ExitCode.Success

  def beeper(latch: CountDownLatch): IO[Unit] =
    for {
      _ <- latch.await
      _ <- IO(s"BEEP! at ${System.currentTimeMillis()}").debug
    } yield ()

  def tickingClock(latch: CountDownLatch): IO[Unit] =
    for {
      _ <- IO.sleep(1.second)
      _ <- IO(System.currentTimeMillis()).debug
      _ <- latch.decrement
      _ <- tickingClock(latch)
    } yield ()
}
