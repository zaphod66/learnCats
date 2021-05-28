package com.zaphod.monad

import cats._

import scala.annotation.tailrec
import scala.language.higherKinds

object OptionTMonadTest {
  implicit val listMonad: Monad[List] = new Monad[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def flatMap[A, B](fa: List[A])(f: A => List[B]): List[B] = fa.flatMap(f)
    override def tailRecM[A, B](a: A)(f: A => List[Either[A, B]]): List[B] = {
      val buf = List.newBuilder[B]
      @tailrec
      def go(lists: List[List[Either[A, B]]]): Unit = lists match {
        case (ab :: abs) :: tail =>
          ab match {
            case Right(b) => buf += b; go(abs :: tail)
            case Left(a)  => go(f(a) :: abs :: tail)
          }
        case Nil :: tail => go(tail)
        case Nil         => ()
      }
      go(f(a) :: Nil)
      buf.result
    }

  }

  val l1: List[Int] = Monad[List].ifM(List(true, false, true))(List(1, 2), List(3, 4))

  // Unlike Functors and Applicatives, not all Monads compose.
  // This means that even if M[_] and N[_] are both Monads, M[N[_]] is not guaranteed to be a Monad.

  case class OptionT[F[_], A](value: F[Option[A]])

  implicit def optionTMonad[F[_]](implicit F: Monad[F]) = {
    //  new Monad[OptionT[F, ?]] {
    type OptionAlias[A] = OptionT[F, A]
    new Monad[OptionAlias] {
      override def pure[A](a: A): OptionT[F, A] = OptionT(F.pure(Some(a)))
      override def flatMap[A, B](fa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] = {
        OptionT {
          F.flatMap(fa.value) {
            case Some(a) => f(a).value
            case None    => F.pure(None)
          }
        }
      }

      override def tailRecM[A, B](a: A)(f: A => OptionAlias[Either[A, B]]): OptionAlias[B] = ???
    }
  }

}
