package com.zaphod.essentialEffects.parallel

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxParallelTraverse1
import com.zaphod.util.Debug._

object ParTraverse extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    tasks.parTraverse(task).debug.as(ExitCode.Success)

  val numTasks = 100
  val tasks: List[Int] = List.range(0, numTasks)

  def task(id: Int): IO[Int] = IO(id).debug
}
