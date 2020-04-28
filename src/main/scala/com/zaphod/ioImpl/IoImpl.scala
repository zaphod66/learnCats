package com.zaphod.ioImpl

import scala.util.control.NonFatal

// implementation of simple (!) IO runtime
// Fabio Labella - How to Fibers work (Scala World 2019)
// see: https://www.youtube.com/watch?v=x5_MmZVLiSM

sealed trait IO[+A]
case class FlatMap[B, +A](io: IO[B], k: B => IO[A]) extends IO[A]
case class Pure[+A](v: A) extends IO[A]
case class Delay[+A](eff: () => A) extends IO[A]

case class RaiseError(e: Throwable) extends IO[Nothing]
case class HandleErrorWith[+A](io: IO[A], f: Throwable => IO[A]) extends IO[A]

object IO {
  implicit class RichList[A](l: List[A]) {
    def push(a: => A): List[A]    = a :: l
    def pop: Option[(A, List[A])] =
      if (l.isEmpty)
        Option.empty[(A, List[A])]
      else
        Option(l.head, l.tail)
  }

  def apply[A](a: => A): IO[A] = Delay( () => a )

  def pure[A](a: A): IO[A] = Pure(a)
  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B] = FlatMap(fa, f)
  def map[A, B](fa: IO[A])(f: A => B): IO[B] = flatMap(fa)(a => pure(f(a)))
  def handleErrorWith[A](fa: IO[A])(f: Throwable => IO[A]) = HandleErrorWith(fa, f)

  def unsafeRunSync[A](io: IO[A]): A = {
    sealed trait Bind {
      def isHandler: Boolean = this.isInstanceOf[Bind.H]
    }
    object Bind {
      case class K(f: Any => IO[Any]) extends Bind
      case class H(f: Throwable => IO[Any]) extends Bind
    }

    def loop(current: IO[Any], stack: List[Bind]): A = current match {
      case FlatMap(io, k) =>
        loop(io, stack.push(Bind.K(k)))
      case HandleErrorWith(io, h) =>
        loop(io, stack.push(Bind.H(h)))
      case Delay(body) =>
        try {
          val res = body()
          loop(pure(res), stack)
        } catch {
          case NonFatal(e) => loop(RaiseError(e), stack)
        }
      case Pure(v) =>
        stack.dropWhile(_.isHandler) match {
          case Nil => v.asInstanceOf[A]
          case Bind.K(f) :: stack => loop(f(v), stack)
        }
      case RaiseError(e) =>
        stack.dropWhile(!_.isHandler) match {
          case Nil => throw e
          case Bind.H(f) :: stack => loop(f(e), stack)
        }
    }

    loop(io, List.empty[Bind])
  }

  implicit class IOSyntax[A](io: IO[A]) {
    def map[B](f: A => B): IO[B] = IO.map(io)(f)
    def flatMap[B](f: A => IO[B]): IO[B] = IO.flatMap(io)(f)
    def >>[B](fb: IO[B]): IO[B] = IO.flatMap(io)(_ => fb)

    def handleErrorWith(f: Throwable => IO[A]): IO[A] = IO.handleErrorWith(io)(f)

    def unsafeRunSync(): A = IO.unsafeRunSync(io)
  }
}

object IoImpl extends App {
  import IO._

  val read: IO[String]   = IO(scala.io.StdIn.readLine)
  def putStrLn[A](v: A): IO[Unit] = IO(println(v))
  val prompt: IO[String] = putStrLn("What is your name?") >> read
  val hello: IO[Unit]    = prompt.flatMap(n => putStrLn(s"Hello $n!"))

  println(s"hello: $hello")

  hello.unsafeRunSync()

  val num1 = IO("boom")
  val boom = num1.map(_.toInt)
  val save = boom.handleErrorWith(_ => IO(0))
  val out1 = save.flatMap(putStrLn)

  out1.unsafeRunSync()
//  boom.flatMap(putStrLn).unsafeRunSync()

  val elem = IO(0)
  val succ = List.fill(10000)(1).foldRight(elem)((i, s) => s.map(_ + i))
  val print = succ.flatMap(i => putStrLn(s"Sum: $i"))

//  println(print)

  val s = System.currentTimeMillis()
  print.unsafeRunSync()
  val e = System.currentTimeMillis()
  println(s"took: " + (e - s))
}
