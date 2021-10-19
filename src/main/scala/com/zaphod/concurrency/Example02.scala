package com.zaphod.concurrency

import cats.effect.{IO, IOApp}

import scala.concurrent.duration.DurationInt

object Example02 extends IOApp.Simple {
  override def run: IO[Unit] =
    for {
      fiber <- (IO.println("Hello!") *> IO.sleep(500.millis)).foreverM.start
      _ <- IO.sleep(2.seconds)
      _ <- fiber.cancel
    } yield ()
}
