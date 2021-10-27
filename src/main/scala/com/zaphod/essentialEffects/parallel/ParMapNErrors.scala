package com.zaphod.essentialEffects.parallel

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxTuple2Parallel
import com.zaphod.util.Debug._

object ParMapNErrors extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    e1.attempt.debug *>
      IO("---").debug *>
      e2.attempt.debug *>
      IO("---").debug *>
      e3.attempt.debug *>
      IO.pure(ExitCode.Success)

  private val ok  = IO("ok").debug
  private val ko1 = IO.raiseError[String](new RuntimeException("ko1")).debug
  private val ko2 = IO.raiseError[String](new RuntimeException("ko2")).debug

  private val e1 = ( ok, ko1).parMapN((_, _) => ())
  private val e2 = (ko1, ok ).parMapN((_, _) => ())
  private val e3 = (ko1, ko2).parMapN((_, _) => ())
}
