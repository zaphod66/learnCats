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

    import cats.instances.list._

    val v4 = Semigroupal[List].product(List(1,2), List(3, 4, 5))

    println(s"v4 = $v4")

    import cats.instances.either._

    type ErrorOr[A] = Either[Vector[String], A]

    val v5 = Semigroupal[ErrorOr].product(Left(Vector("Error 1")), Left(Vector("Error 2")))

    println(s"v5 = $v5")
  }

  object Exercise {
    import scala.language.higherKinds

    import cats.Monad
    import cats.syntax.flatMap._
    import cats.syntax.functor._

    def product[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
      for {
        a <- ma
        b <- mb
      } yield (a, b)
    }

    def product2[M[_]: Monad, A, B](ma: M[A], mb: M[B]): M[(A, B)] = {
      ma.flatMap { a =>
        mb.flatMap { b =>
          Monad[M].pure( (a, b) )
        }
      }
    }
  }

  object Validation {
    import cats.Semigroupal
    import cats.data.Validated
    import cats.instances.list._

    type AllErrorsOr[A] = Validated[List[String], A]

    val s1 = Semigroupal[AllErrorsOr].product(
      Validated.Invalid(List("Error1")),
      Validated.Invalid(List("Error2"))
    )

    val v1 = Validated.valid[List[String], Int](123)
    val i1 = Validated.invalid[List[String], Int](List("Boom"))

    import cats.syntax.validated._

    val v2 = 123.valid[List[String]]
    val i2 = List("Boom").invalid[Int]

    import cats.syntax.applicative._
    import cats.syntax.applicativeError._

    val v3 = 123.pure[AllErrorsOr]
    val i3 = List("Boom").raiseError[AllErrorsOr, String]

    type ErrorOr[A] = Validated[String, A]

    import cats.instances.string._
    import cats.syntax.apply._    // for .tupled

    val i4 = ("Error 1".invalid[Int], "Error 2".invalid[Int]).tupled

    println(s"v1: $v1\tv2: $v2\tv3: $v3")
    println(s"i1: $i1\ti2: $i2\ti3: $i3\ti4: $i4")

    val m1 = 123.valid[String].map(_ * 10)
    val m2 = "?".invalid[Int].leftMap(">" + _ + "<")
    val m3 = 123.valid[String].bimap(">" + _ + "<", _ * 10)
    val m4 = "?".invalid[Int].bimap(">" + _ + "<", _ * 10)

    import cats.syntax.either._

    val e1 = "Boom".invalid[Int].toEither
    val d1 = e1.toValidated
    val d2 = v1.withEither(e => e.flatMap(i => Right(i + 1)))
    val e2 = e1.withValidated(v => v.bimap(">" + _ + "<", _ * 10))

    println(s"e1: $e1\td1: $d1\td2: $d2\te2: $e2")

    val r1 = i1.getOrElse(0)
    val r2 = i1.fold(_.mkString(">", "-", "<"), _.toString)

    println(s"r1: $r1\tr2: $r2")
  }

  object ValidatedExercise {
    import cats.data.Validated
    import cats.syntax.validated._

    case class User(name: String, age: Int)

    type FormData = Map[String,String]  // imagine it comes from a web form
    type FailFast[A] = Either[List[String], A]
    type FailSlow[A] = Validated[List[String], A]

    def getValue(name: String)(data: FormData): FailFast[String] = data.get(name).toRight(List(s"$name not specified!"))

    def getName = getValue("name") _

    import cats.syntax.either._

    def parseInt(name: String)(data: String): FailFast[Int] =
      Either.catchOnly[NumberFormatException](data.toInt).leftMap(_ => List(s"$name not an Int ($data)"))

    val e1 = "Mona".asRight[String]


    def nonBlank(name: String)(data: String): FailFast[String] = {
      data.valid[List[String]].toEither.ensure(List(s"$name must not be blank"))(_.nonEmpty)
    }

    def nonNegative(name: String)(data: Int): FailFast[Int] = {
      data.valid[List[String]].toEither.ensure(List(s"$name must be non-negative"))(_ >= 0)
    }

    val v1 = getName(Map.empty[String, String])
    val v2 = getName(Map("name" -> "Mona"))
    val v3 = parseInt("age")("11")
    val v4 = parseInt("age")("foo")
    val v5 = nonBlank("name")("Dude")
    val v6 = nonBlank("name")("")
    val v7 = nonNegative("age")(11)
    val v8 = nonNegative("age")(-1)

    println(s"v1: $v1\tv2: $v2\tv3: $v3\tv4: $v4\tv5: $v5\tv6: $v6\tv7: $v7\tv8: $v8")

    def readName(vals: FormData): FailFast[String] =
      getValue("name")(vals).flatMap(nonBlank("name"))

    def readAge(vals: FormData): FailFast[Int] =
      getValue("age")(vals).
        flatMap(nonBlank("age")).
        flatMap(parseInt("age")).
        flatMap(nonNegative("age"))

    val v9 = readName(Map("name" -> "Mona"))
    val va = readName(Map("name" -> ""))
    val vb = readName(Map())
    val vc = readAge(Map("age" -> "11"))
    val vd = readAge(Map("age" -> "-1"))
    val ve = readAge(Map())

    println(s"v9: $v9\tva: $va\tvb: $vb")
    println(s"vc: $vc\tvd: $vd\tve: $ve")

    import cats.instances.list._
    import cats.syntax.apply._

    def readUser(data: FormData): FailSlow[User] = {
      (
        readName(data).toValidated,
        readAge(data).toValidated
      ).mapN(User.apply)
    }

    val vf = readUser(Map("name" -> "Mona", "age" -> "11"))
    val vg = readUser(Map("name" -> "Mona", "age" -> "-1"))
    val vh = readUser(Map("name" -> "",     "age" -> "11"))
    val vi = readUser(Map("age" -> "-1"))

    println(s"vf: $vf\tvg: $vg\tvh: $vh\tvi: $vi")
  }

//  Basics
//  Apply

  Validation
  ValidatedExercise
}
