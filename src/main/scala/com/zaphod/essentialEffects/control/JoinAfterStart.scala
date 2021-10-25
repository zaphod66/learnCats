package com.zaphod.essentialEffects.control

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.DurationInt

import com.zaphod.util.Debug._

object JoinAfterStart extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      fiber <- task.start
      _     <- IO("pre-join").debug
      _     <- fiber.join.debug
      _     <- IO("post-join").debug
    } yield ExitCode.Success

  val task: IO[String] = IO.sleep(200.millis) *> IO("task").debug
}
