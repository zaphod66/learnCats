package com.zaphod.essentialEffects.effects

import cats.effect.{ExitCode, IO, IOApp}

import scala.concurrent.duration.{DurationInt, FiniteDuration, MILLISECONDS}

object Timing extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    timedHello.flatMap {
      case (dur, _) => IO.println(s"'hello' took $dur")
    }.as(ExitCode.Success)

  val clock: IO[Long] = IO(System.currentTimeMillis)
  def time[A](action: IO[A]): IO[(FiniteDuration, A)] =
    for {
      start <- clock
      a     <- action
      end   <- clock
    } yield (FiniteDuration(end - start, MILLISECONDS), a)

  val timedHello = time(IO.sleep(1.second) *> IO.println("hello"))
}
