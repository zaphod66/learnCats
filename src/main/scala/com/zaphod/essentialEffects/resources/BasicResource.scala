package com.zaphod.essentialEffects.resources

import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.zaphod.util.Debug.DebugHelper

object BasicResource extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    stringResource.use { s =>
      IO(s"$s is so cool!").debug
    }.as(ExitCode.Success)

  val stringResource: Resource[IO, String] =
    Resource.make(
      IO("> acquiring resource").debug *> IO("String")
    )(
      _ => IO("< releasing resource").debug.void
    )
}
