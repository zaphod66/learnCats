package com.zaphod.essentialEffects.resources

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.toFunctorOps
import com.zaphod.util.Debug.DebugHelper

import scala.concurrent.duration.DurationInt

object BackgroundTaskResource extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    prog1 *> IO("---").debug *> prog2.as(ExitCode.Success)

  private val loop = (IO("looping...").debug *> IO.sleep(100.millis)).foreverM

  private val backgroundTask: Resource[IO, Unit] = {
    Resource
      .make(
        IO("> forking backgroundTask").debug *> loop.start
      )(
        IO("< cancelling backgroundTask").debug *> _.cancel
      ).void
  }
  val prog1 = for {
    _ <- backgroundTask.use { _ =>
      IO("other work while running a background task").debug *>
        IO.sleep(300.millis) *>
        IO("other work done").debug
    }
    _ <- IO("all done").debug
  } yield ()

  val prog2 = for {
    _ <- loop.background.use { _ =>
      IO("other work while running a background task").debug *>
        IO.sleep(300.millis) *>
        IO("other work done").debug
    }
  } yield ()
}
