package com.zaphod.essentialEffects.asynchrony

import cats.effect.{ExitCode, IO, IOApp}
import com.zaphod.util.Debug.DebugHelper

object Never extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    never.guarantee(IO("I guess never is now.").debug.void)
      .as(ExitCode.Success)

  val never: IO[Nothing] = IO.async_(_ => ())
}
