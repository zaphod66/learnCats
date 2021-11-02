package com.zaphod.essentialEffects.asynchrony

import cats.effect.{ExitCode, IO, IOApp}
import com.zaphod.util.Debug.DebugHelper

import java.util.concurrent.CompletableFuture
import scala.jdk.FunctionConverters.enrichAsJavaBiFunction

object AsyncCompletable extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    IO("main start!").debug *> effect.debug *> IO("main end!").debug.as(ExitCode.Success)

  val effect: IO[String] = fromCF(IO(cf()))

  def fromCF[A](cfa: IO[CompletableFuture[A]]): IO[A] =
    cfa.flatMap { fa =>
      IO.async_ { cb =>
        val handler: (A, Throwable) => Unit = {
          case (a, null) => cb(Right(a))
          case (null, t) => cb(Left(t))
          case (a, t)    => sys.error(s"CompletableFuture returned $a AND $t")
        }

        fa.handle(handler.asJavaBiFunction)

        ()
      }
    }

  def cf(): CompletableFuture[String] =
    CompletableFuture.supplyAsync(() => { Thread.sleep(100); s"[${Thread.currentThread.getName}] woo!" })
}
