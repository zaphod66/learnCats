package com.zaphod.typeLambda

import cats._

object TypeLambdaTest extends App {
  implicit class ListFunctor[A](xs: List[A]) extends Functor[List] {
    override def map[A, B](fa: List[A])(f: (A) => B): List[B] = fa map f
  }

  case class Box[A](a: A)
}
