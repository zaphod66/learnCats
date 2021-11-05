package com.zaphod.essentialEffects.coordination

import cats.effect.{ExitCode, IO, IOApp, Ref}
import cats.implicits.{catsSyntaxApplicative, catsSyntaxParallelTraverse1}
import com.zaphod.util.Debug.DebugHelper

object RefUpdateImpure extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    prog1 *> IO("---").debug *> prog2.as(ExitCode.Success)
  }

  def prog(task: (Int, Ref[IO, Int]) => IO[Unit]): IO[Unit] =
    for {
      ref <- Ref[IO].of(0)
      _   <- List(1, 2, 3).parTraverse(task(_, ref))
    } yield ()

  val prog1 = prog(taskImpure)
  val prog2 = prog(taskPure)

  def taskImpure(id: Int, ref: Ref[IO, Int]): IO[Unit] =
    ref.modify(prev => id -> println(s"$prev -> $id"))
      .replicateA(3)
      .void

  def taskPure(id: Int, ref: Ref[IO, Int]): IO[Unit] =
    ref.modify(prev => id -> IO(s"$prev -> $id").debug)
      .flatten
      .replicateA(3)
      .void
}
