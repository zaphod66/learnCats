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
  println("IoTest")

  val testDB = new DB {
    override def retrieveInt(): IO[Int] = IO(42)
    override def retrieveStr(): IO[String] = IO("Mona")
  }

  val plainId  = UserId(_)
  val plainStr = Username(_)

  val liftedId  = Functor[IO].lift(plainId)
  val liftedStr = Functor[IO].lift(plainStr)

  val retrievedUserId   = liftedId(testDB.retrieveInt())
  val retrievedUsername = liftedStr(testDB.retrieveStr())

  val retrievedUser = Applicative[IO].map2(retrievedUserId, retrievedUsername)(User)

  val user = retrievedUser.unsafeRunSync()

  println(s"User: $user")
}
