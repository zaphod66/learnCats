package com.zaphod.UnderScore

object Chapter02_Monoid extends App {
  import cats.Monoid

  val booleanAndMonoid = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = x && y
  }

  val booleanOrMonoid = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = x || y
  }

  val booleanXorMonoid = new Monoid[Boolean] {
    override def empty: Boolean = false
    override def combine(x: Boolean, y: Boolean): Boolean = (x && !y) || (!x && y)
  }

  val booleanNXorMonoid = new Monoid[Boolean] {
    override def empty: Boolean = true
    override def combine(x: Boolean, y: Boolean): Boolean = (!x && y) || (x && !y)
  }

  implicit def setUnionMonoid[A] = new Monoid[Set[A]] {
    override def empty: Set[A] = Set.empty[A]
    override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
  }

  implicit val intMonoid = new Monoid[Int] {
    override def empty: Int = 0
    override def combine(x: Int, y: Int): Int = x + y
  }

  val intSetMonoid = Monoid[Set[Int]]

  val s = intSetMonoid.combine(Set(1, 2), Set(2, 3))

  println(s"s = $s")

  import cats.instances.option._
  import cats.syntax.semigroup._

  val o3 = Monoid[Option[Int]].combine(Option(12), Option(30))
  val o4 = Option(12) |+| Option(30)

  println(s"o3 = $o3")
  println(s"o4 = $o4")

  def addAll[A : Monoid](items: List[A]): A = items.foldLeft(Monoid[A].empty)(_ |+| _)
}
