package com.zaphod.essentialEffects.coordination

import cats.effect.{Deferred, ExitCode, IO, IOApp, Ref}
import cats.implicits.catsSyntaxTuple2Parallel
import com.zaphod.util.Debug.DebugHelper

trait CountDownLatch {
  def await: IO[Unit]
  def decrement: IO[Unit]
}

object CountDownLatch {
  def apply(count: Long): IO[CountDownLatch] =
    for {
      whenDone <- Deferred[IO, Unit]
      state    <- Ref[IO].of[State](Outstanding(count, whenDone))
    } yield new CountDownLatch {
      override def await: IO[Unit] =
        state.get.flatMap {
          case Outstanding(_, whenDone) => whenDone.get
          case Done()                   => IO.unit
        }

      override def decrement: IO[Unit] =
        state.modify {
          case Outstanding(1, whenDone) =>
            Done() -> whenDone.complete(()) *> IO.unit
          case Outstanding(n, whenDone) =>
            Outstanding(n - 1, whenDone) -> IO.unit
          case Done() =>
            Done() -> IO.unit
        }.flatten.uncancelable
    }

  sealed trait State
  case class Outstanding(n: Long, whenDone: Deferred[IO, Unit]) extends State
  case class Done() extends State
}

object RunCountDownLatch extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    prepareAndRun.as(ExitCode.Success)

  def actionWithPrerequisites(latch: CountDownLatch): IO[String] =
    for {
      _      <- IO("Waiting for prerequisites").debug
      _      <- latch.await
      result <- IO("action").debug
    } yield result

  def runPrerequisite(latch: CountDownLatch): IO[String] =
    for {
      result <- IO("Prerequisite").debug
      _      <- latch.decrement
    } yield result

  val prepareAndRun: IO[Unit] =
    for {
      latch <- CountDownLatch(1)
      _ <- (actionWithPrerequisites(latch), runPrerequisite(latch)).parTupled
    } yield ()
}
