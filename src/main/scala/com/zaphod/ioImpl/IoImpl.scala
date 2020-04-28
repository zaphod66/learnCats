package com.zaphod.ioImpl

// implementation of simple (!) IO runtime
// Fabio Labella - How to Fibers work (Scala World 2019)
// see: https://www.youtube.com/watch?v=x5_MmZVLiSM

// isomorphic to '() => A'
//trait IO[_] {
//  def delay[A](a: => A): IO[A]
//
//  def pure[A](a: A): IO[A]
//  def flatMap[A, B](fa: IO[A])(f: A => IO[B]): IO[B]
//
//  def unsafeRunSync[A](fa: IO[A]): A
//}

// Data type

sealed trait IO[+A]
case class FlatMap[B, +A](io: IO[B], k: B => IO[A]) extends IO[A]
case class Pure[+A](v: A) extends IO[A]
case class Delay[+A](eff: () => A) extends IO[A]

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

  def unsafeRunSync[A](io: IO[A]): A = {
    def loop(current: IO[Any], stack: List[Any => IO[Any]]): A = current match {
      case FlatMap(io, k) =>
        loop(io, stack.push(k))
      case Delay(body) =>
        val res = body()
        loop(pure(res), stack)
      case Pure(v) =>
        stack.pop match {
          case None                => v.asInstanceOf[A]
          case Some((bind, stack)) =>
            val nextIO = bind(v)
            loop(nextIO, stack)
        }
    }

    loop(io, List.empty[Any => IO[Any]])
  }

  implicit class IOSyntax[A](io: IO[A]) {
    def map[B](f: A => B): IO[B] = IO.map(io)(f)
    def flatMap[B](f: A => IO[B]): IO[B] = IO.flatMap(io)(f)
    def >>[B](fb: IO[B]): IO[B] = IO.flatMap(io)(_ => fb)
    def unsafeRunSync(): A = IO.unsafeRunSync(io)
  }
}

object IoImpl extends App {
  import IO._

  val read: IO[String]       = IO(scala.io.StdIn.readLine)
  def printStrLn[A](v: A): IO[Unit] = IO(println(v))
  val prompt: IO[String]     = printStrLn("What is your name?") >> read
  val hello: IO[Unit]        = prompt.flatMap(n => printStrLn(s"Hello $n!"))

//  println(s"hello: $hello")

//  hello.unsafeRunSync()

  val elem = IO(0)
  val succ = List.fill(10000)(1).foldRight(elem)((i, s) => s.map(_ + i))
  val print = succ.flatMap(i => printStrLn(s"Sum: $i"))

//  println(print)

  val s = System.currentTimeMillis()
  print.unsafeRunSync()
  val e = System.currentTimeMillis()
  println(s"took: " + (e - s))
}
