package com.zaphod.stuff

import java.util.Random
import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object RandomTest extends App {
  val random = new Random()
  val seed = 4242

  random.setSeed(seed)

  private val f = Future(random.nextInt(10))

  val z1 = for {
    x <- f
    y <- f
  } yield x + y

  random.setSeed(seed)

  val z2 = for {
    x <- Future(random.nextInt(10))
    y <- Future(random.nextInt(10))
  } yield x + y

  val r1 = Await.result(z1, Duration.Inf)
  val r2 = Await.result(z2, Duration.Inf)

  println(s"r1: $r1")
  println(s"r2: $r2")
}
