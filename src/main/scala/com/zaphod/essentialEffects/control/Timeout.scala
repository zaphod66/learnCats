package com.zaphod.essentialEffects.control

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.{DurationInt, FiniteDuration}
import com.zaphod.util.Debug._

object Timeout extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      won <- IO.race(task, timeout)
      _   <- won match {
        case Left(_)  => IO("   task: won").debug
        case Right(_) => IO("timeout: won").debug
      }
    } yield ExitCode.Success

  val task: IO[Unit]    = annotatedSleep("   task", 100.millis)
  val timeout: IO[Unit] = annotatedSleep("timeout", 500.millis)

  def annotatedSleep(name: String, duration: FiniteDuration): IO[Unit] =
    (
      IO(s"$name: starting").debug *>
      IO.sleep(duration) *>
      IO(s"$name: done").debug
    ).onCancel(IO(s"$name: canceled").debug.void).void
}
