package com.zaphod.effect

import scala.concurrent.duration.TimeUnit
import scala.language.higherKinds

object ClockTest extends App {
  import cats.effect._
  import cats.implicits._

  import scala.concurrent.duration.{MILLISECONDS, NANOSECONDS}

  def measure[F[_], A](fa: F[A])(implicit F: Sync[F], clock: Clock[F]): F[(A, Long)] = {
    for {
      start <- clock.monotonic(MILLISECONDS)
      result <- fa
      finish <- clock.monotonic(MILLISECONDS)
    } yield (result, finish - start)
  }

  def fib(n: Int): IO[Int] = {
    IO.suspend {
//      println(s"fib($n)")
      if (n <= 1) IO.pure(n)
      else (fib(n - 1), fib(n - 2)).mapN(_ + _)
    }
  }

  implicit val clockIO: Clock[IO] = new Clock[IO] {
    override def realTime(unit: TimeUnit): IO[Long] = {
      println(s"realTime($unit)")
      IO(unit.convert(System.currentTimeMillis(), MILLISECONDS))
    }

    override def monotonic(unit: TimeUnit): IO[Long] = {
      println(s"monotonic($unit)")
      IO(unit.convert(System.nanoTime(), NANOSECONDS))
    }
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
  println(s"r20: $r36")
}
