package com.zaphod.concurrency

import cats.effect.kernel.Deferred
import cats.effect.{IO, IOApp}
import cats.syntax.all._

import scala.concurrent.duration.DurationInt

object Example06 extends IOApp.Simple {

  sealed trait State
  final case class Awaiting(latches: Int, waiter: Deferred[IO, Unit]) extends State
  case object Released extends State

  trait Latch {
    def release: IO[Unit]
    def await: IO[Unit]
  }

  object Latch {
    def apply(latches: Int): IO[Latch] =
      for {
        waiter <- IO.deferred[Unit]
        state  <- IO.ref[State](Awaiting(latches, waiter))
      } yield new Latch {
        override def release: IO[Unit] =
          state.modify {
            case Awaiting(n, w) =>
              if (n > 1)
                (Awaiting(n - 1, w), IO.unit)
              else
                (Released, waiter.complete(()))
            case Released => (Released, IO.unit)
          }.flatten.void

        override def await: IO[Unit] =
          state.get.flatMap {
            case Released       => IO.unit
            case Awaiting(_, w) => w.get
          }
      }
  }

  override def run: IO[Unit] = {
    val num = 9
    for {
      latch <- Latch(num)
      _     <- (1 to num).toList.traverse { idx => (IO.println(s"$idx counting down")
                  *> IO.sleep(250.millis) *> latch.release).start }
      _     <- latch.await
      _     <- IO.println("Got past the latch")
    } yield ()
  }
}
