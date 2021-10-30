package com.zaphod.essentialEffects.contexts

//import cats.effect.unsafe.implicits.global
import cats.effect.{ExitCode, IO, IOApp}
import com.zaphod.util.Colorize
import com.zaphod.util.Debug._

object Blocker extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    withBlocker.as(ExitCode.Success)

  val withBlocker: IO[Unit] =
    for {
      _ <- IO("on default1").debug
      _ <- IO.blocking(s"on blocker [${Colorize(Thread.currentThread.getName)}]").debug
      _ <- IO("on default2").debug
    } yield ()
}
