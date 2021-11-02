package com.zaphod.essentialEffects.resources

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.{catsSyntaxTuple2Parallel, catsSyntaxTuple2Semigroupal}
import com.zaphod.util.Debug.DebugHelper

object BasicResourceComposed extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    composedResource1.use(f) *> IO("---").debug *>
      composedResource2.use(f).as(ExitCode.Success)

  def f: ((String, Int)) => IO[Unit] = { case (s, i) =>
    IO(s"$s is so cool").debug *>
      IO(s"$i is also cool").debug.void
  }

  private val stringResource: Resource[IO, String] =
    Resource.make(
      IO("> acquiring stringResource").debug *> IO("String")
    )(
      _ => IO("< releasing stringResource").debug.void
    )

  private val intResource: Resource[IO, Int] =
    Resource.make(
      IO("> acquiring intResource").debug *> IO(42)
    )(
      _ => IO("< releasing intResource").debug.void
    )

  private val composedResource1 = (stringResource, intResource).tupled
  private val composedResource2 = (stringResource, intResource).parTupled

}
