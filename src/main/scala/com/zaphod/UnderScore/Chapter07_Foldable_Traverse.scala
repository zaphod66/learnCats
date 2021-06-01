package com.zaphod.UnderScore

object Chapter07_Foldable_Traverse extends App {
  object Basics {
    val s1 = List(1, 2, 3).foldLeft(0)(_ + _)
    val s2 = List(1, 2, 3).foldLeft(0)(_ - _)
    val s3 = List(1, 2, 3).foldRight(0)(_ - _)

    val l1 = List(1, 2, 3).foldLeft(List.empty[Int])((acc, i) => i::acc)
    val l2 = List(1, 2, 3).foldRight(List.empty[Int])((i, acc) => i::acc)

    def map[A, B](l: List[A])(f: A => B): List[B] =
      l.foldRight(List.empty[B])((i, acc) => f(i) :: acc)
    def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
      l.foldRight(List.empty[B])((i, acc) => f(i) ::: acc)
    def filter[A](l: List[A])(f: A => Boolean): List[A] =
      l.foldRight(List.empty[A])((i, acc) => if (f(i)) i :: acc else acc)

    def sumNumeric[A](l: List[A])(implicit num: Numeric[A]): A = l.foldRight(num.zero)(num.plus)

    import cats.Monoid

    def sumMonoid[A](l: List[A])(implicit mon: Monoid[A]): A = l.foldRight(mon.empty)(mon.combine)
  }

  object FoldableUse {
    import cats.Foldable
    import cats.instances.list._

    val ints = List(1, 2, 3)

    val f1 = Foldable[List].foldLeft(ints, 0)(_ + _)

    import cats.instances.option._

    val o1 = Option(123)
    val f2 = Foldable[Option].foldLeft(o1, 10)(_ * _)

    import cats.instances.stream._
    import cats.Eval

    def bigData: LazyList[Int] = (1 to 100000).to(LazyList)

    val s1 = try {
      bigData.foldRight(0L)(_ + _).toString
    } catch {
      case t: Throwable => t.toString
    }

    val e1 = Foldable[LazyList].foldRight(bigData, Eval.now(0L))( (i, acc) => acc.map(_ + i) ).value

    println(s"s1: $s1\te1: $e1")

    val m1 = Foldable[Option].nonEmpty(o1)
    val m2 = Foldable[List].find(ints)(_ % 2 == 0)

    import cats.instances.int._

    val m3 = Foldable[List].combineAll(ints)

    import cats.instances.string._

    val m4 = Foldable[List].foldMap(ints)(_.toString)

    import cats.instances.vector._

    val nested = List(Vector(1, 2, 3), Vector(4, 5, 6))

    val m5 =(Foldable[List] compose Foldable[Vector]).combineAll(nested)

    import cats.syntax.foldable._

    val m6 = ints.combineAll

    println(s"m1: $m1\tm2: $m2\tm3: $m3\tm4: $m4\tm5: $m5\tm6: $m6")
  }

  object TraverseOwn {
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global

    val hostNames = List(
      "alpha.example.com",
      "beta.example.com",
      "gamma.example.com"
    )

    def getUptime(hostName: String): Future[Int] = Future(hostName.length * 60)   // just for the sake

    val allUptimes1 = Future.traverse(hostNames)(getUptime)

    import cats.Applicative
    import cats.instances.future._
    import cats.syntax.applicative._
    import cats.syntax.apply._

    val zero = List.empty[Int].pure[Future]
    def combine(acc: Future[List[Int]], host: String): Future[List[Int]] = (acc, getUptime(host)).mapN(_ :+ _)

    def listTraverse[F[_]: Applicative, A, B](as: List[A])(f: A => F[B]): F[List[B]] =
      as.foldLeft(List.empty[B].pure[F]) { (acc, a) =>
        println(s"acc: $acc, a: $a, f(a): ${f(a)} => ${(acc, f(a)).mapN(_ :+ _)}")
        (acc, f(a)).mapN(_ :+ _)
      }

    def listSequence[F[_]: Applicative, A](as: List[F[A]]): F[List[A]] = listTraverse(as)(identity)

    val allUptimes2 = listTraverse(hostNames)(getUptime)
  }

  object TraverseExercise {
    import TraverseOwn.{listTraverse, listSequence}


    import cats.instances.vector._

    val v1 = listSequence(List(Vector(1, 2), Vector(3, 4)))
    val v2 = listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))

    println(s"v1: $v1")
    println(s"v2: $v2")

    import cats.instances.option._

    def process1(l: List[Int]) = listTraverse(l)(n => if (n % 2 == 0) Some(n) else None)

    val p1 = process1(List(2, 4, 6))
    val p2 = process1(List(1, 2, 3))

    println(s"p1: $p1")
    println(s"p2: $p2")

    import cats.data.Validated
    import cats.instances.list._

    type ErrorOr[A] = Validated[List[String], A]

    def process2(l: List[Int]): ErrorOr[List[Int]] = listTraverse(l) { n =>
      if (n % 2 == 0)
        Validated.valid[List[String],Int](n)
      else
        Validated.invalid[List[String],Int](List(s"$n is not even"))
    }

    val p3 = process2(List(2, 4, 6))
    val p4 = process2(List(1, 2, 3))

    println(s"p3: $p3")
    println(s"p4: $p4")
  }

  object TraverseUse {
    import scala.concurrent.Future
    import scala.concurrent.ExecutionContext.Implicits.global

    import cats.Traverse
    import cats.instances.future._
    import cats.instances.list._

    import TraverseOwn.{hostNames, getUptime}

    val totalUptime1: Future[List[Int]] = Traverse[List].traverse(hostNames)(getUptime)

    val nf1 = List(Future(1), Future(2), Future(3))
    val nf2: Future[List[Int]] = Traverse[List].sequence(nf1)

    import cats.syntax.traverse._

    val totalUptime2: Future[List[Int]] = hostNames.traverse(getUptime)
    val nf3: Future[List[Int]] = nf1.sequence
  }

  Basics
  FoldableUse

  import TraverseOwn._

  import scala.concurrent._
  import scala.concurrent.duration._

  println(s"allUptimes1: ${Await.result(allUptimes1, 1.second)}")
  println(s"allUptimes2: ${Await.result(allUptimes2, 1.second)}")

  TraverseExercise

  import TraverseUse._

  println(s"totalUptime1: ${Await.result(totalUptime1, 1.second)}")
  println(s"totalUptime2: ${Await.result(totalUptime2, 1.second)}")
  println(s"nf2: ${Await.result(nf2, 1.second)}")
  println(s"nf3: ${Await.result(nf3, 1.second)}")
}
