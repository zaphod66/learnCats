package com.zaphod.UnderScore

object Chapter10_Validation extends App {

  import cats.Semigroup
  import cats.syntax.semigroup._
  import cats.data.Validated
  import cats.data.Validated._
  import cats.syntax.validated._
  import cats.syntax.apply._

  object Pred {
    sealed trait Predicate[E, A] {
      import Predicate._

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        this match {
          case Pure(f)   => f(a)
          case And(l, r) => (l(a), r(a)).mapN((_, _) => a)
          case Or(l, r)  => l(a) match {
            case Valid(_)   => Valid(a)
            case Invalid(el)   => r(a) match {
              case Valid(_)    => Valid(a)
              case Invalid(er) => Invalid(el |+| er)
            }
          }
        }

      def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)
      def or(that: Predicate[E, A]): Predicate[E, A]  = Or(this, that)
    }

    object Predicate {
      final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A]

      final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
      final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]

      def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)
      def lift[E, A](e: E, f: A => Boolean): Predicate[E, A] = Pure { a => if (f(a)) a.valid else e.invalid }
    }
  }

  object CheckTrait {
    import Pred._

    sealed trait Check[E, A, B] {
      import Check._

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B]

      def map[C](f: B => C): Check[E, A, C] = Map[E, A, B, C](this, f)
      def flatMap[C](f: B => Check[E, A, C]): Check[E, A, C] = FlatMap[E, A, B, C](this, f)

      def andThen[C](next: Check[E, B, C]): Check[E, A, C] = AndThen[E, A, B, C](this, next)
    }

    object Check {
      final case class Pure[E, A, B](f: A => Validated[E, B]) extends Check[E, A, B] {
        override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, B] = f(a)
      }

      final case class Map[E, A, B, C](check: Check[E, A, B], f: B => C) extends Check[E, A, C] {
        override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = check(a).map(f)
      }

      final case class FlatMap[E, A, B, C](check: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
        override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = check(a).withEither(_.flatMap(b => f(b)(a).toEither))
      }

      final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
        override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] = check1(a).withEither(_.flatMap(b => check2(b).toEither))
      }

      final case class PurePredicate[E, A](p: Predicate[E, A]) extends Check[E, A, A] {
        override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = p(a)
      }

      def apply[E, A](p: Predicate[E, A]): Check[E, A, A] = PurePredicate(p)
      def apply[E, A, B](f: A => Validated[E, B]): Check[E, A, B] = Pure(f)
    }
  }

  object PredicateUse {
    import Pred.Predicate._
    import Pred._

    val a: Predicate[List[String], Int] = Pure { v => if (v > 2) v.valid else List("Must be > 2").invalid }
    val b: Predicate[List[String], Int] = Pure { v => if (v < 0) v.valid else List("Must be < 0").invalid }

    val c: Predicate[List[String], Int] = a and b
    val d: Predicate[List[String], Int] = a or b

    import cats.instances.list._

    val c1 = c(5)
    val c2 = c(1)
    val c3 = c(-1)
    val c4 = d(5)
    val c5 = d(1)
    val c6 = d(-1)

    println(s"AND: c1: $c1, c2: $c2, c3: $c3")
    println(s"OR:  c4: $c4, c5: $c5, c6: $c6")
  }

  object CheckUse {
    import cats.data.NonEmptyList
    import cats.syntax.validated._

    import Pred._
    import CheckTrait._

    type Errors = NonEmptyList[String]

    def error(s: String): Errors = NonEmptyList(s, Nil)

    def longerThan(n: Int): Predicate[Errors, String] = Predicate.lift(
      error(s"Must be longer than $n chars"),
      s => s.length > n
    )

    def alphaNumeric: Predicate[Errors, String] = Predicate.lift(
      error(s"Must be alphanumeric"),
      s => s.forall(_.isLetterOrDigit)
    )

    def contains(c: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain '$c'"),
      s => s.contains(c)
    )

    def containsOnce(c: Char): Predicate[Errors, String] = Predicate.lift(
      error(s"Must contain '$c' only once"),
      s => s.count(ch => ch == c) == 1
    )

    final case class User(username: String, email: String)

    val checkUsername: Check[Errors, String, String] = Check(longerThan(3) and alphaNumeric)

    val splitEmail: Check[Errors, String, (String, String)] = Check[Errors, String, (String, String)] { s =>
      s.split('@') match {
        case Array(name, domain) => (name, domain).validNel[String]
        case _                   => "Must contain a single '@'".invalidNel[(String, String)]
      }
    }

    val checkLeft: Check[Errors, String, String] = Check(longerThan(0))
    val checkRight: Check[Errors, String, String] = Check(longerThan(3) and contains('.'))

    val joinEmail: Check[Errors, (String, String), String] = Check {
      case (l, r) => (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

    val checkEmail: Check[Errors, String, String] = splitEmail andThen joinEmail

    def createUser(username: String, email: String): Validated[Errors, User] =
      (checkUsername(username), checkEmail(email)).mapN(User)
  }

  PredicateUse
}
