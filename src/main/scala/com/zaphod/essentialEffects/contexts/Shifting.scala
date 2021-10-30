package com.zaphod.essentialEffects.contexts

import cats.effect.{ExitCode, IO, IOApp, Spawn}
import com.zaphod.util.Debug._
object Shifting extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO("One").debug
      _ <- IO.cede  //  Spawn[IO].cede
      _ <- IO("Two").debug
      _ <- IO.cede  //  Spawn[IO].cede
      _ <- Spawn[IO].cede
      _ <- IO("Three").debug
    } yield ExitCode.Success
}
