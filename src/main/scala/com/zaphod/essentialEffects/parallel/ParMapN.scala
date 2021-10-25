package com.zaphod.essentialEffects.parallel

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Parallel
import com.zaphod.util.Debug._

object ParMapN extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    par.as(ExitCode.Success)

  private val hello = IO("hello").debug
  private val world = IO("world").debug

  private val par = (hello, world).parMapN((h, w) => s"$h $w").debug
}
