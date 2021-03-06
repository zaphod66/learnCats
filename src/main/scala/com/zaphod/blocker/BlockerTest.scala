package com.zaphod.blocker

import cats.effect._

trait JDBC[F[_]] {
  def execute: F[Unit]
}

object JDBC {
  def mock[F[_]: Sync]: JDBC[F] = new JDBC[F] {
    override def execute: F[Unit] = Sync[F].blocking {
      println("executing...")
      Thread.sleep(1000)
    }
  }

  def mockIO: JDBC[IO] = new JDBC[IO] {
    override def execute: IO[Unit] = IO.blocking {
      println("executing...")
      Thread.sleep(1000)
    }
  }
}

object BlockerTest extends IOApp {
  val jdbc1: JDBC[IO] = JDBC.mock[IO]
  val jdbc2: JDBC[IO] = JDBC.mockIO

  def putStrLn(s: String): IO[Unit] = IO(println(s))
  def wrap(jdbc: JDBC[IO]): IO[Unit] = for {
    _ <- putStrLn("Start")
    _ <- jdbc.execute
    _ <- putStrLn("End")
  } yield ()

  def run(args: List[String]): IO[ExitCode] = for {
    _ <- wrap(jdbc1)
    _ <- wrap(jdbc2)
  } yield ExitCode.Success
}
