package com.zaphod.concurrency

import cats.effect.{IO, IOApp}
import cats.syntax.all._

object Example01 extends IOApp.Simple {
  def repeat(letter: String): IO[Unit] =
    IO.print(letter).replicateA(50).void

  override def run: IO[Unit] =
    for {
      fa <- (repeat("A") *> repeat("B")).as("foo!").start
      fb <- (repeat("C") *> repeat("D")).as("bar!").start

      ra <- fa.joinWithNever
      rb <- fb.joinWithNever

      _ <- IO.println(s"\ndone: a says: $ra, b says: $rb")
    } yield ()
}
