package com.zaphod.effect

import scala.language.higherKinds

object RefTest extends App {
  import cats.effect.{IO, Sync}
  import cats.effect.concurrent.Ref
  import cats.implicits._

  import scala.concurrent.ExecutionContext
  implicit val ctx = IO.contextShift(ExecutionContext.global)

  class Worker[F[_]](number: Int, ref: Ref[F, Int])(implicit F: Sync[F]) {

    private def putStrLn(value: String): F[Unit] = F.delay(println(value))

    def increment: F[Unit] = {
      for {
        c1 <- ref.get
        _ <- putStrLn(s"$number >> $c1")
        c2 <- ref.modify(x => (x + 1, x))
        _ <- putStrLn(s"$number >> $c2")
      } yield ()
    }
  }

  val program: IO[Unit] = {
    for {
      ref <- Ref.of[IO, Int](0)
      w1  = new Worker[IO](1, ref)
      w2  = new Worker[IO](2, ref)
      w3  = new Worker[IO](3, ref)

      _ <- List(
        w1.increment,
        w2.increment,
        w3.increment
      ).parSequence.void
    } yield ()
  }

  program.unsafeRunSync()
}
