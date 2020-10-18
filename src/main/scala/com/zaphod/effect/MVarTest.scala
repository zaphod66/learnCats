package com.zaphod.effect

import cats.effect.{Fiber, ContextShift, IO}
import cats.effect.concurrent.MVar

// Doesn't work
object MVarTest extends App {

  import scala.concurrent.ExecutionContext

  implicit val contextShift: ContextShift[IO] = IO.contextShift(ExecutionContext.global)

  object ProducerConsumer {
    type Channel[A] = MVar[IO, Option[A]]

    def producer(channel: Channel[Int], list: List[Int]): IO[Unit] = {
      println(s"producer($list)")
      list match {
        case Nil    =>
          println(s"-")
          channel.put(None)   // done!
        case h :: t =>
          println(s"$h::$t")
          val cp = channel.put(Some(h))
          println(s"$h")
          cp.flatMap { _ => println(s"h"); producer(channel, t) }
      }
    }

    def consumer(channel: Channel[Int], sum: Long): IO[Long] = {
      println(s"consumer($sum)")
      channel.take.flatMap {
        case Some(x) => println(s"$x"); consumer(channel, sum + x)
        case None    => println(s"-");  IO.pure(sum)
      }
    }

    val sum = for {
      channel <- MVar[IO].empty[Option[Int]]
      count = 10

      producerTask = producer(channel, (0 until count).toList)
      consumerTask = consumer(channel, 0L)

      fp  <- producerTask.start
      fc  <- consumerTask.start

      _   <- fp.join
      sum <- fc.join
    } yield sum

    println("==================")
    println(s"sum: ${sum.unsafeRunSync()}")
    println("==================")
  }

  ProducerConsumer
}
