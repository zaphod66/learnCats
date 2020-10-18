package com.zaphod.stuff

import scala.concurrent.{Await, CanAwait, ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration
import scala.util.Try

case class MyFuture[T](fut: Future[T]) extends Future[T] {

  println("ctor")

  override def onComplete[U](f: Try[T] => U)(implicit executor: ExecutionContext): Unit = fut.onComplete(f)
  override def isCompleted: Boolean = fut.isCompleted
  override def value: Option[Try[T]] = fut.value
  override def transform[S](f: Try[T] => Try[S])(implicit executor: ExecutionContext): MyFuture[S] = new MyFuture(fut.transform(f))
  override def transformWith[S](f: Try[T] => Future[S])(implicit executor: ExecutionContext): MyFuture[S] = new MyFuture(fut.transformWith(f))
  override def ready(atMost: Duration)(implicit permit: CanAwait): MyFuture.this.type = ???
  override def result(atMost: Duration)(implicit permit: CanAwait): T = fut.result(atMost)
  override def equals(that: Any): Boolean = fut.equals(that)


  override def map[S](f: T => S)(implicit executor: ExecutionContext): MyFuture[S] = {
    println("map")
    MyFuture(fut.map(f))
  }

  override def flatMap[S](f: T => Future[S])(implicit executor: ExecutionContext): MyFuture[S] = {
    println("flatMap")
    MyFuture(fut.flatMap(f))
  }
}

object MyFuture {
  val unit: MyFuture[Unit] = successful(())
  def failed[T](exception: Throwable): MyFuture[T] = new MyFuture(Promise.failed(exception).future)
  def successful[T](result: T): MyFuture[T] = new MyFuture(Promise.successful(result).future)

  def apply[T](body: =>T)(implicit executor: ExecutionContext): MyFuture[T] = {
    unit.map(_ => body)
  }
}

object FutureTest extends App {
  import scala.concurrent.ExecutionContext.Implicits.global

  println("======== construction =========")
  val w = MyFuture(1)
  val x = MyFuture(2)
  val y = MyFuture(3)
  println("========      for     =========")
  val z = for {
    h <- w
    i <- x
    j <- y
    k = i + j
  } yield h + k


  val r = Await.result(z, Duration.Inf)

  println(s"r: $r")
}
