package com.zaphod.essentialEffects.parallel

import cats.Parallel
import cats.effect.implicits.commutativeApplicativeForParallelF
import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.{catsSyntaxTuple2Parallel, catsSyntaxTuple2Semigroupal}
import com.zaphod.util.Debug._

object ParMapN extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    par1 *> par2.as(ExitCode.Success)

  private val hello = IO("hello").debug
  private val world = IO("world").debug

  private val par1 = (hello, world).parMapN((h, w) => s"$h $w").debug

  // doing by hand the conversion of IO to Parallel and back,
  // which is done by parMapN automatically
  private val pHello: IO.Par[String] = Parallel[IO].parallel(hello)
  private val pWorld: IO.Par[String] = Parallel[IO].parallel(world)

  private val pPar2 = (pHello, pWorld).mapN((h, w) => s"$h $w")

  private val par2 = Parallel[IO].sequential(pPar2).debug
}
