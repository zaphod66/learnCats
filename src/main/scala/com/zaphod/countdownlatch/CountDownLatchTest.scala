package com.zaphod.countdownlatch

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.blocking

object CountDownLatchJava extends App {
  import java.util.concurrent.CountDownLatch

  val threadCount = 4
  val iterations = 100000
  var results = Map.empty[Int, Int]

  println("Started ...")

  for (_ <- 0 until iterations) {
    val consumersStarted  = new CountDownLatch(threadCount)
    val mainThreadReady   = new CountDownLatch(1)
    val consumersFinished = new CountDownLatch(threadCount)

    var sharedState = 0

    for (_ <- 0 until threadCount)
      global.execute(() => {
        blocking {
          consumersStarted.countDown()
          mainThreadReady.await()

          // workload (obviously wrong!)
          sharedState += 1

          consumersFinished.countDown()
        }
      })

    consumersStarted.await()
    mainThreadReady.countDown()
    consumersFinished.await()

    results = results.updated(sharedState, results.getOrElse(sharedState, 0) + 1)
  }

  println("Done")
  println(results /*.toList.sortBy(_._2).reverse.take(5).mkString(" ") */)
}

object CountDownLatchTest extends App {

}
