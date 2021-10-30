package com.zaphod.essentialEffects.contexts

import cats.effect.{Async, ExitCode, IO, IOApp}
import com.zaphod.util.Debug.DebugHelper

import java.util.concurrent.Executors
import scala.concurrent.ExecutionContext

object ShiftingMultiple extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    (ec("1"), ec("2")) match {
      case (ec1, ec2) =>
        for {
          _ <- IO("--- 1").debug
          _ <- Async[IO].evalOn(IO("One").debug, ec1)
          _ <- IO("--- 2").debug
          _ <- Async[IO].evalOn(IO("Two").debug, ec2)
          _ <- IO("--- 3").debug
        } yield ExitCode.Success
    }

  def ec(name: String): ExecutionContext =
    ExecutionContext.fromExecutor(Executors.newSingleThreadExecutor { r =>
      val t = new Thread(r, s"pool-$name-tread-1")
      t.setDaemon(true)
      t
    })
}
