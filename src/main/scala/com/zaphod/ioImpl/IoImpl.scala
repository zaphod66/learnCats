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
  def apply[A](a: => A): IO[A] = Delay( () => a )

  def pure[A](a: A): IO[A] = Pure(a)
  def flatMap[A,B](fa: IO[A])(f: A => IO[B]): IO[B] = FlatMap(fa, f)
}

object IoImpl extends App {
  def read = IO(scala.io.StdIn.readLine)
  def put[A](v: A) = IO(println(v))
  def prompt = IO.flatMap(put("What is your name?"))(_ => read)
  def hello = IO.flatMap(prompt)(n => put(s"Hello $n!"))

  println(s"hello: $hello")
}
