package com.zaphod.essentialEffects.parallel

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxParallelSequence1
import com.zaphod.util.Debug._

object ParSequence extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    tasks.parSequence.debug.as(ExitCode.Success)

  val numTasks = 100
  val tasks: List[IO[Int]] = List.tabulate(numTasks)(task)

  def task(id: Int): IO[Int] = IO(id).debug
}
