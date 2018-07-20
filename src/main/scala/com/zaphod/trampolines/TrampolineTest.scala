package com.zaphod.trampolines

object TrampolineTest extends App {

  object Data {
    val list1: List[Char] = List.fill(100)('x')
    val list2: List[Int]  = List.fill(1000001)(0)
  }

  ////////////////////////////////////////////////////////////

  object Plain {
    import Data._

    def even[A](as: List[A]): Boolean = {
      as match {
        case Nil     => true
        case _ :: xs => odd(xs)
      }
    }

    def odd[A](as: List[A]): Boolean = {
      as match {
        case Nil     => false
        case _ :: xs => even(xs)
      }
    }

    println(s"plain  list1 => ${even(list1)}")

    try {
      println(s"plain  list2 => ${even(list2)}")
    } catch {
      case e: StackOverflowError => println(s"Exception: $e")
    }

  }

  ////////////////////////////////////////////////////////////

  object SimpleTrampoline {

    sealed trait Trampoline[+A] {
      @annotation.tailrec
      final def run: A = this match {
        case Done(a) => a
        case More(k) => k().run
      }
    }

    case class Done[+A](a: A) extends Trampoline[A]
    case class More[+A](k: () => Trampoline[A]) extends Trampoline[A]

    def even[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil     => Done(true)
      case _ :: xs => More( () => odd(xs) )
    }

    def odd[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil     => Done(false)
      case _ :: xs => More( () => even(xs) )
    }

    import Data._

    println(s"simple list1 => ${even(list1).run}")
    println(s"simple list2 => ${even(list2).run}")

  }

  ////////////////////////////////////////////////////////////

  object ScalaTrampoline {

    import scala.util.control.TailCalls._

    def even[A](as: List[A]): TailRec[Boolean] = as match {
      case Nil     => done(true)
      case _ :: xs => tailcall(odd(xs))
    }

    def odd[A](as: List[A]): TailRec[Boolean] = as match {
      case Nil     => done(false)
      case _ :: xs => tailcall(even(xs))
    }

    import Data._

    println(s"scala  list1 => ${even(list1).result}")
    println(s"scala  list2 => ${even(list2).result}")

  }

  ////////////////////////////////////////////////////////////

  object CatsTrampoline {

    import cats.Eval

    def even[A](as: List[A]): Eval[Boolean] = as match {
      case Nil => Eval.now(true)
      case _ :: xs => Eval.defer(odd(xs))
    }

    def odd[A](as: List[A]): Eval[Boolean] = as match {
      case Nil => Eval.now(false)
      case _ :: xs => Eval.defer(even(xs))
    }

    import Data._

    println(s"cats   list1 => ${even(list1).value}")
    println(s"cats   list2 => ${even(list2).value}")
  }

  object CatsFoldRight {

    import cats.Eval

    def foldRightNaive[A, B](as: List[A], acc: B)(f: (A, B) => B): B = {
      as match {
        case Nil    => acc
        case h :: t => f(h, foldRightNaive(t, acc)(f))
      }
    }

    def foldRight[A, B](as: List[A], acc: B)(f: (A, B) => B): B = {
      def go(as: List[A], acc: Eval[B])(f: (A, Eval[B]) => Eval[B]): Eval[B] = as match {
        case h :: t => Eval.defer(f(h, go(t, acc)(f)))
        case Nil    => acc
      }

      go(as, Eval.now(acc)) { (a, b) => b.map( f(a, _) ) }.value
    }

    import Data._

    val sum1 = try {
      foldRightNaive(list2, 0)((e, acc) => e + acc)
    } catch {
      case e: StackOverflowError => { println(s"Exception: $e"); 0 }
    }
    val sum2 = foldRight(list2, 0)((e, acc) => e + acc)

    println(s"cats foldRight => $sum1")
    println(s"cats foldRight => $sum2")

  }

  ////////////////////////////////////////////////////////////

  object ScalazTrampoline {

    import scalaz.Free.Trampoline
    import scalaz.Trampoline._

    def even[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil     => done(true)
      case _ :: xs => suspend(odd(xs))
    }

    def odd[A](as: List[A]): Trampoline[Boolean] = as match {
      case Nil     => done(false)
      case _ :: xs => suspend(even(xs))
    }

    import Data._

    println(s"scalaz list1 => ${even(list1).run}")
    println(s"scalaz list2 => ${even(list2).run}")
  }

  ////////////////////////////////////////////////////////////

  
  ////////////////////////////////////////////////////////////

  Plain
  SimpleTrampoline
  ScalaTrampoline
  CatsTrampoline
  ScalazTrampoline

  CatsFoldRight
}
