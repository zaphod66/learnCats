package com.zaphod.essentialEffects.contexts

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxParallelTraverse1
import com.zaphod.util.Debug._

object Parallelism extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    for {
      _ <- IO(s"number of CPUs: $numCPU").debug
      _ <- tasks.debug
    } yield ExitCode.Success

  val numCPU = Runtime.getRuntime.availableProcessors
  val tasks  = List.range(0, numCPU * 2).parTraverse(task)
  def task(i: Int): IO[Int] = IO(i).debug
}
