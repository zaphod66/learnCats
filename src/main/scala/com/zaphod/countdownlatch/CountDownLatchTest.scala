package com.zaphod.countdownlatch

import java.util.concurrent.RejectedExecutionException
import scala.concurrent.duration.{Duration, DurationInt, FiniteDuration}
import scala.concurrent.{Await, ExecutionContext, TimeoutException, blocking}

object CountDownLatchJava extends App {
  import java.util.concurrent.CountDownLatch
  import scala.concurrent.ExecutionContext.Implicits.global

  val threadCount = 4
  val iterations = 10000
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

  println("... Done")
  println(results /*.toList.sortBy(_._2).reverse.take(5).mkString(" ") */)
}

object CountDownLatchASync extends App {
  import scala.annotation.tailrec

  import java.util.concurrent.atomic.AtomicReference
  import java.util.concurrent.Executors

  import scala.concurrent.{ Future, Promise }

  final class ASyncCountDownLatch(count: Int)(implicit ec: ExecutionContext) extends AutoCloseable {

    private[this] val state = new AtomicReference[State](State(count, Set.empty))
    private final case class State(count: Int, tasks: Set[Promise[Unit]])

    private[this] val scheduler = Executors.newSingleThreadScheduledExecutor( (r: Runnable) => {
      val t = new Thread(r)
      t.setDaemon(true)
      t.setName(s"CountDownLatch-${hashCode()}")

      t
    })

    private def installTimeout(p: Promise[Unit], timeout: FiniteDuration): Unit = {
      @tailrec
      def removePromise(): Unit = {
        state.get() match {
          case current @State(_, tasks) =>
            val update = current.copy(tasks = tasks - p)
            if (!state.compareAndSet(current, update))
              removePromise()
            else
              ()  // CountDownLatch reach zero
          case _ => ()
        }
      }

      val ex = new TimeoutException(s"AsyncCountDownLatch.await($timeout)")
      val timeoutTask: Runnable = () => {
        removePromise()
        p.tryFailure(ex)
      }

      try {
        val cancelToken = scheduler.schedule(timeoutTask, timeout.length, timeout.unit)

        p.future.onComplete { r =>
          if (r.fold(_ != ex, _ => true))
            cancelToken.cancel(false)
        }
      } catch {
        case _: RejectedExecutionException => ()
      }
    }

    override def close(): Unit = {
      state.lazySet(null) // GC purposes
      scheduler.shutdown()
    }

    @tailrec
    def countDown(): Unit =
      state.get() match {
        case current @ State(count, tasks) if count > 0 =>
          val update = State(count - 1, tasks)
          if (!state.compareAndSet(current, update)) {
            countDown() // retry
          } else if (update.count == 0) {
            ec.execute( () => {
              for (r <- tasks) r.trySuccess(())
              close()
            })
          }
        case _ => ()
      }


    @tailrec
    def await(timeout: Duration): Future[Unit] =
      state.get() match {
        case current @State(count, tasks) if count > 0 =>
          val p = Promise[Unit]()
          val update = State(count, tasks + p)

          timeout match {
            case d: FiniteDuration => installTimeout(p, d)
            case _ => ()
          }

          if (!state.compareAndSet(current, update)) {
            await(timeout) // retry
          } else {
            p.future
          }
        case _ => Future.unit
      }
  }

  // --------------------------------

  val threadCount = 4
  val iterations = 100000
  var results = Map.empty[Int, Int]

  import scala.concurrent.ExecutionContext.Implicits.global

  println("Started ...")
  for (_ <- 0 until iterations) {
    val consumersStarted = new ASyncCountDownLatch(threadCount)
    val mainThreadReady  = new ASyncCountDownLatch(1)
    val consumersFinished = new ASyncCountDownLatch(threadCount)

    var sharedState = 0

    for (_ <- 0 until threadCount) {
      for {
        _ <- Future.unit
        _ =  consumersStarted.countDown()
        _ <- mainThreadReady.await(Duration.Inf)
      } yield {
        sharedState += 1
        consumersFinished.countDown()
      }
    }

    val await = for {
      _ <- consumersStarted.await(Duration.Inf)
      _ =  mainThreadReady.countDown()
      _ <- consumersFinished.await(1.milli)
    } yield {
      results = results.updated(sharedState, results.getOrElse(sharedState, 0) + 1)
    }

    try {
      Await.result(await, Duration.Inf)
    } catch {
      case _: TimeoutException =>
        println("Timeout...")
        results.updated(0, results.getOrElse(0, 0) + 1)
    }
  }

  println("... Done")
  println(results)
}

object CallbackHell extends App {
  import scala.concurrent.ExecutionContext.global

  type Async[A] = (A => Unit) => Unit

  def timesTwoAsync(n: Int): Async[Int] =
    onFinish => {
      global.execute(() => {
        val res = n * 2
        onFinish(res)
      })
    }

  // Usage
  timesTwoAsync(21) { result => println(s"res: $result") }

  def timesFourAsync(n: Int): Async[Int] =
    onFinish => {
      timesTwoAsync(n) { a =>
        timesTwoAsync(n) { b =>
          onFinish(a + b)
        }
      }
    }

  timesFourAsync(21) { result => println(s"res: $result") }

  Thread.sleep(5)

  println("Done")
}
