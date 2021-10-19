package com.zaphod.UnderScore

object Chapter03_Functor extends App {
  object Basics {
    import cats.Functor
    import cats.instances.list._
    import cats.instances.option._

    val l1 = List(1, 2, 3)
    val l2 = Functor[List].map(l1)(_ * 2)

    val o1 = Option(123)
    val o2 = Functor[Option].map(o1)(_ * 2)

    val f1 = (x: Int) => x + 1
    val liftedF1 = Functor[Option].lift(f1)

    val o3 = liftedF1(o1)

    import cats.instances.function._
    import cats.syntax.functor._

    val f2 = (x: Int) => x * 2
    val f3 = (x: Int) => s"$x!"
    val f4 = f1.map(f2).map(f3)

    println(s"f4(123) = ${f4(123)}")

    def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
      start.map(n => (n + 1) * 2)

    val d1 = doMath(o1)
    val d2 = doMath(l1)

    println(s"doMath(o1) = $d1")
    println(s"doMath(l1) = $d2")

    final case class Box[A](value: A)
    implicit val boxFunctor: Functor[Box] = new Functor[Box] {
      override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box[B](f(fa.value))
    }

    val b1 = Box[Int](123)
    val b2 = b1.map(value => value + 1)

    println(s"b1 = $b1\tb2 = $b2")
  }

  object BranchingWithFunctors {
    import cats.Functor
    import cats.syntax.functor._

    sealed trait Tree[+A]
    final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]
    final case class Leaf[A](value: A) extends Tree[A]

    object Tree {
      def leaf[A](a: A): Tree[A] = Leaf(a)
      def branch[A](l: Tree[A], r: Tree[A]): Tree[A] = Branch(l, r)
    }

    implicit val treeFunctor: Functor[Tree] = new Functor[Tree] {
      override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
        case Leaf(a)      => Leaf(f(a))
        case Branch(l, r) => Branch(l.map(f), r.map(f))
      }
    }

    import Tree._
    val t1 = branch(leaf(10), leaf(20))
    val t2 = t1.map(_ * 2)

    println(s"t1 = $t1\tt2 = $t2")
  }

  object Contravariant {
    trait Printable[A] {
      self =>

      def format(a: A): String

      def contramap[B](f: B => A): Printable[B] = new Printable[B] {
        override def format(b: B): String = self.format(f(b))
      }
    }

    def format[A](a: A)(implicit p: Printable[A]): String = p.format(a)

    implicit val stringPrintable: Printable[String] = new Printable[String] {
      override def format(a: String): String = "\"" + a + "\""
    }

    implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
      override def format(a: Boolean): String = if (a) "yes" else "no"
    }

    val f1 = format("hello")

    final case class Box[A](value: A)

    implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] = p.contramap[Box[A]](_.value)

    val f2 = format(Box("Mona"))
    val f3 = format(Box(true))

    println(s"f1 = $f1\tf2 = $f2\tf3 = $f3")
  }

  object Invariant {
    trait Codec[A] {
      self =>

      def encode(a: A): String
      def decode(s: String): A

      def imap[B](dec: A => B, enc: B => A): Codec[B] = new Codec[B] {
        override def encode(b: B): String = self.encode(enc(b))
        override def decode(s: String): B = dec(self.decode(s))
      }
    }

    def encode[A](a: A)(implicit c: Codec[A]): String = c.encode(a)
    def decode[A](s: String)(implicit c: Codec[A]): A = c.decode(s)

    implicit val stringCodec: Codec[String] = new Codec[String] {
      override def encode(s: String): String = s
      override def decode(s: String): String = s
    }

    implicit val doubleCodec: Codec[Double] = stringCodec.imap(_.toDouble, _.toString)

    case class Box[A](value: A)

    implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] = c.imap(Box(_), _.value)

    val e1 = encode(123.4)
    val d1 = decode[Double]("123.4")
    val e2 = encode(Box(234.5))
    val d2 = decode[Box[Double]]("234.5")

    println(s"e1 = $e1\td1 = $d1\te2 = $e2\td2 = $d2")
  }

  Basics
  BranchingWithFunctors
  Contravariant
  Invariant
}
