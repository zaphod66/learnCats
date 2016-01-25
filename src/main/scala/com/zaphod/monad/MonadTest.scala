package com.zaphod.monad

import cats._

import scala.language.higherKinds

object MonadTest extends App {
  implicit def optionMonad(implicit app: Applicative[Option]) = {
    new Monad[Option] {
      override def pure[A](a: A): Option[A] = app.pure(a)
      override def flatMap[A, B](ma: Option[A])(f: A => Option[B]): Option[B] =
        app.map(ma)(f).flatten
    }
  }

  implicit val listMonad = new Monad[List] {
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
  }

  val l1 = Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4))

  // Unlike Functors and Applicatives, not all Monads compose.
  // This means that even if M[_] and N[_] are both Monads, M[N[_]] is not guaranteed to be a Monad.

  case class OptionT[F[_], A](value: F[Option[A]])

  implicit def optionTMonad[F[_]](implicit F: Monad[F]) = {
//  new Monad[OptionT[F, ?]] {
    type OptionAlias[A] = OptionT[F, A]
    new Monad[OptionAlias] {
      def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
      def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = {
        OptionT {
          F.flatMap(fa.value) {
            case Some(a) => f(a).value
            case None    => F.pure(None)
          }
        }
      }
    }
  }

}
