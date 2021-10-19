package com.zaphod.concurrency

import cats.effect.{IO, IOApp}

object Example03 extends IOApp.Simple{
  def fac(n: Long): Long =
    if (n == 0) 1 else n * fac(n - 1)

  override def run: IO[Unit] =
    for {
      res <- IO.race(IO(fac(20)), IO(fac(20)))
      _   <- res.fold(
        a => IO.println(s"Left hand side won: $a"),
        b => IO.println(s"Right hand side won: $b")
      )
    } yield ()
}
