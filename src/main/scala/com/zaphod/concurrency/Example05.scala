package com.zaphod.concurrency

import cats.effect.kernel.Deferred
import cats.effect.{IO, IOApp}

import scala.concurrent.duration.DurationInt

object Example05 extends IOApp.Simple {
  def countDown(s: String, n: Int, pause: Int, waiter: Deferred[IO, Unit]): IO[Unit] =
    IO.println(s"$s: $n") *> IO.defer {
      if (n == 0) IO.println(s"$s done.") *> IO.unit
      else if (n == pause) IO.println(s"$s paused...") *> waiter.get *> countDown(s, n - 1, pause, waiter)
      else countDown(s, n - 1, pause, waiter)
    }

  override def run: IO[Unit] =
    for {
      waiter <- IO.deferred[Unit]
      f <- countDown("f", 10, 5, waiter).start
      g <- countDown("g",  8, 5, waiter).start
      _ <- IO.sleep(5.seconds)
      _ <- waiter.complete(())
      _ <- f.join
      _ <- g.join
      _ <- IO.println("blast off!")
    } yield ()
}
