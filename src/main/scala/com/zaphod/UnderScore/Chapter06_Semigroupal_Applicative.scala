package com.zaphod.UnderScore

object Chapter06_Semigroupal_Applicative extends App {
  object Basics {
    import cats.Semigroupal
    import cats.instances.option._

    val s1 = Semigroupal[Option].product(Some(123), Some("abc"))
    val s2 = Semigroupal[Option].product(Option.empty[Int], Some("abc"))
    val s3 = Semigroupal[Option].product(Some(123), Option.empty[String])
    val s4 = Semigroupal.tuple3(Option(1), Option(2), Option(3))
    val v1 = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)

    println(s"s1 = $s1")
    println(s"s2 = $s2")
    println(s"s3 = $s3")
    println(s"s4 = $s4")
    println(s"v1 = $v1")
  }

  object Apply {
    import cats.Semigroupal
    import cats.instances.option._
    import cats.syntax.apply._

    val s5 = (Option(1), Option(2), Option(3)).tupled

    println(s"s5 = $s5")

    case class Cat(name: String, born: Int, color: String)

    val v2 = (Option("Garfield"), Option(1978), Option("Orange & Black")).mapN(Cat.apply)

    println(s"v2 = $v2")

    import cats.Monoid
    import cats.instances.int._
    import cats.instances.string._
    import cats.syntax.semigroup._

    val tupleToCat: (String, Int, String) => Cat = Cat.apply
    val catToTuple: Cat => (String, Int, String) = c => (c.name, c.born, c.color)

    implicit val semiMonoid = new Semigroupal[Monoid] {
      override def product[A, B](fa: Monoid[A], fb: Monoid[B]): Monoid[(A, B)] = new Monoid[(A, B)] {
        override def empty: (A, B) = (fa.empty, fb.empty)
        override def combine(x: (A, B), y: (A, B)): (A, B) = (fa.combine(x._1, y._1), fb.combine(x._2, y._2))
      }
    }

    implicit val catMonoid: Monoid[Cat] = (Monoid[String], Monoid[Int], Monoid[String]).imapN(tupleToCat)(catToTuple)

    val garfield   = Cat("Garfield",   1978, "Orange & Black")
    val heathcliff = Cat("Heathcliff", 1988, "Orange")

    val v3 = garfield |+| heathcliff

    println(s"v3 = $v3")

  }

  Basics
  Apply
}
