package com.zaphod.blocker

import java.util.concurrent.Executors
/*
import cats.effect.{ContextShift, ExitCode, IO, IOApp, Resource, Sync, Blocker}

import scala.concurrent.ExecutionContext
import scala.language.higherKinds

trait JDBC[F[_]] {
  def execute: F[Unit]
}

object JDBC {
  def mock[F[_]: ContextShift: Sync](blockingExecutionContext: ExecutionContext): JDBC[F] = new JDBC[F] {
    override def execute: F[Unit] = ContextShift[F].evalOn(blockingExecutionContext)(Sync[F].delay(println("executing...")))
  }

  def mockBlocker[F[_]: ContextShift: Sync](blocker: Blocker): JDBC[F] = new JDBC[F] {
//    override def execute: F[Unit] = ContextShift[F].evalOn(blocker.blockingContext)(Sync[F].delay(println("executing..."))) // or: the same:
    override def execute: F[Unit] = blocker.blockOn(Sync[F].delay(println("executing...")))
  }
}

object BlockerTest extends IOApp{

  import cats.syntax.functor._

  val blockingExecutionContextResource: Resource[IO, ExecutionContext] = Resource.make {
    IO {
      ExecutionContext.fromExecutorService(Executors.newCachedThreadPool())
    }
  } {
    ec => IO(ec.shutdown())
  }.widen

  val jdbc: Resource[IO, JDBC[IO]] = blockingExecutionContextResource.map { blockingEC =>
    JDBC.mock[IO](blockingEC)
  }

  val blockerResource: Resource[IO, Blocker] = Blocker.apply[IO]

  val jdbcBlocker: Resource[IO, JDBC[IO]] = Blocker[IO].map { blocker =>
    JDBC.mockBlocker[IO](blocker)
  }

  override def run(args: List[String]): IO[ExitCode] = {
//    jdbc.use(_.execute).as(ExitCode.Success)
    jdbcBlocker.use(_.execute).as(ExitCode.Success)
  }
}
*/
