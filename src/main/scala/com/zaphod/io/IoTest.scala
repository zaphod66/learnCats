package com.zaphod.io

import cats.effect.IO
import cats.implicits._, cats.effect.implicits._
import cats.Functor
import cats.Applicative

case class Username(name: String)
case class UserId(id: Int)
case class User(id: UserId, name: Username)

trait DB {
  def retrieveInt(): IO[Int]
  def retrieveStr(): IO[String]
}

object IoTest extends App {

  def fib(n: Int, a: Long, b: Long): IO[Long] =
    IO.suspend {
      if (n > 0)
        fib(n - 1, b, a + b)
      else
        IO.pure(a)
    }

  def func2(n: Int, acc: Int): IO[Int] =
    if (n > 0)
      func2(n - 1, acc + 1)
    else
      IO.pure(acc)

  println("IoTest")

  val f1 = fib(20, 0, 1)
  val l1 = f1.unsafeRunSync()

  val f2 = func2(20000, 0)
  val l2 = f2.unsafeRunSync()

  println(s"l1: $l1")
  println(s"l2: $l2")

  val testDB = new DB {
    override def retrieveInt(): IO[Int] = IO(42)
    override def retrieveStr(): IO[String] = IO("Mona")
  }

  val plainId  = UserId(_)
  val plainStr = Username(_)

  val liftedId   = Functor[IO].lift(plainId)
  val liftedName = Functor[IO].lift(plainStr)

  val retrievedUserId   = liftedId(testDB.retrieveInt())
  val retrievedUsername = liftedName(testDB.retrieveStr())

  val retrievedUser = Applicative[IO].map2(retrievedUserId, retrievedUsername)(User)

  val user = retrievedUser.unsafeRunSync()

  println(s"User: $user")
}
