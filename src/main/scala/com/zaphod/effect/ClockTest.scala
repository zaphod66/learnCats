package com.zaphod.effect

import cats.Applicative

import scala.concurrent.duration.{FiniteDuration, TimeUnit}
import cats.effect.unsafe.implicits.global

object ClockTest extends App {
  import cats.effect._
  import cats.implicits._

  import scala.concurrent.duration.{MILLISECONDS, NANOSECONDS}

  def measure[F[_], A](fa: F[A])(implicit F: Sync[F], clock: Clock[F]): F[(A, FiniteDuration)] = {
    for {
      start <- clock.monotonic
      result <- fa
      finish <- clock.monotonic
    } yield (result, finish - start)
  }

  def fib(n: Long): IO[Long] = {
    IO.defer {
//      println(s"fib($n)")
      if (n <= 1) IO.pure(n)
      else (fib(n - 1), fib(n - 2)).mapN(_ + _)
    }
  }

  implicit val clockIO: Clock[IO] = new Clock[IO] {
    override def realTime: IO[FiniteDuration] = {
      val time = System.currentTimeMillis()
      println(s"realTime($time)")
      IO(FiniteDuration(time, MILLISECONDS))
    }

    override def monotonic: IO[FiniteDuration] = {
      val nano = System.nanoTime()
      println(s"monotonic($nano)")
      IO(FiniteDuration(nano, NANOSECONDS))
    }

    override def applicative: Applicative[IO] = ???
  }

//  implicit val clockIO: Clock[IO] = Clock.create[IO]

  val f10 = fib(10)
  val f12 = fib(12)
  val f36 = fib(36)

  println("----------")
  val m10 = measure(f10)
  println("----------")
  val m12 = measure(f12)
  println("----------")
  val m36 = measure(f36)
  println("----------")

  println("++++++++++")
  val r10 = m10.unsafeRunSync()
  println("++++++++++")
  val r12 = m12.unsafeRunSync()
  println("++++++++++")
  val r36 = m36.unsafeRunSync()
  println("++++++++++")

  println(s"r10: $r10")
  println(s"r12: $r12")
  println(s"r36: $r36")
}
