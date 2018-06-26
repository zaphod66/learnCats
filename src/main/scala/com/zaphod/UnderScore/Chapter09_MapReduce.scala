package com.zaphod.UnderScore

object Chapter09_MapReduce extends App {
  object Exercise {
    import cats.Monoid
    import cats.syntax.monoid._

    def foldMap[A, B: Monoid](as: Vector[A])(f: A => B): B = {
//      as.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
      as.foldLeft(Monoid[B].empty)(_ |+| f(_))
    }

    import cats.instances.int._
    import cats.instances.string._

    val v1 = Vector(1, 2, 3)
    val b1 = foldMap(v1)(identity)
    val b2 = foldMap(v1)(_.toString + "!")
    val b3 = foldMap("Hello World!".toVector)(_.toString.toUpperCase)

    println(s"b1: $b1")
    println(s"b2: $b2")
    println(s"b3: $b3")

    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    def parFoldMap[A, B: Monoid](as: Vector[A])(f: A => B): Future[B] = {
      val numCores  = Runtime.getRuntime.availableProcessors()
      val groupSize = (1.0 * as.size / numCores).ceil.toInt

      val batches   = as.grouped(groupSize)

      val futures   = batches map { batch =>
        Future {
          foldMap(batch)(f)
        }
      }

      Future.sequence(futures) map { b =>
        b.foldLeft(Monoid[B].empty)(_ |+| _)
      }
    }

    val v2 = (1 to 1000000).toVector

    val f1 = parFoldMap(v2)(identity)
  }

  object FoldMapWithCats {
    import cats.Monoid
    import cats.Traverse

    import cats.instances.int._
    import cats.instances.future._
    import cats.instances.vector._

    import cats.syntax.foldable._
    import cats.syntax.traverse._

    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    def parFoldMap[A, B: Monoid](as: Vector[A])(f: A => B): Future[B] = {
      val numCores  = Runtime.getRuntime.availableProcessors()
      val groupSize = (1.0 * as.size / numCores).ceil.toInt

      as.grouped(groupSize)
        .toVector
        .traverse(batch => Future(batch.foldMap(f)))
        .map(_.combineAll)
    }

    val v2 = (1 to 1000000).toVector

    val f1 = parFoldMap(v2)(identity)
  }

  Exercise

  import scala.concurrent.Await
  import scala.concurrent.duration._

  val b4 = Await.result(Exercise.f1, 1.second)
  val b5 = Await.result(FoldMapWithCats.f1, 1.second)

  println(s"b4: $b4")
  println(s"b5: $b5")
}
