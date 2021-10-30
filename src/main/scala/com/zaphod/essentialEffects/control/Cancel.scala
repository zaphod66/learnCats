package com.zaphod.essentialEffects.control

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Parallel
import com.zaphod.util.Debug._

import java.util.Date
import scala.concurrent.duration.DurationInt

object Cancel extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    prog1 *> IO("---").debug *> prog2 *> IO("---").debug.as(ExitCode.Success)

  private val prog1 = for {
      fiber <- task.onCancel(IO("canceled").debug.void).start
      _     <- IO("pre-cancel").debug
      _     <- fiber.cancel
      _     <- IO("post-cancel").debug
    } yield ()

  def task: IO[String] = IO("task").debug *> IO.never

  //noinspection ForwardReference
  private val prog2 =
    for {
      _     <- IO(()) // to avoid init error (NPE)
      fiber <- tickingClock.start
      _     <- IO.sleep(4.seconds)
      _     <- fiber.cancel
    } yield ()

  val tickingClock: IO[Unit] =
    for {
      _ <- IO(System.currentTimeMillis).map(ts2Str).debug
      _ <- IO.sleep(1.second)
      _ <- tickingClock
    } yield ()

  def ts2Str(timestamp: Long): String = new Date(timestamp).toString
}
