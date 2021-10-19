package com.zaphod.UnderScore

import scala.util.Random

object Chapter11_CRDTs extends App {

  object Start {
    final case class GCounter(counters: Map[String, Int]) {
      def increment(machine: String, amount: Int): GCounter = {
        val value = amount + counters.getOrElse(machine, 0)
        GCounter(counters + (machine -> value))
      }

      def merge(that: GCounter): GCounter = {
        val merged = that.counters ++ this.counters.map {
          case (k, v) => k -> (v max that.counters.getOrElse(k, 0))
        }
        GCounter(merged)
      }

      def total: Int = counters.values.sum
    }
  }

  object StartUse {
    import Start._

    import cats.Monoid

    implicit val gcounterMonoid = new Monoid[GCounter] {
      override def empty: GCounter = GCounter(Map.empty[String, Int])
      override def combine(x: GCounter, y: GCounter): GCounter = x merge y
    }

    val machines = List("mach1", "mach2", "mach3","mach4", "mach5", "mach6","mach7", "mach8", "mach9")

    val gc0 = GCounter(Map.empty[String, Int])
    val gc1 = gc0.increment(machines(1), 10)
    val gc2 = gc1.increment(machines(2), 20)
    val gc3 = gc2.increment(machines(1), 30)
    val gc4 = gc3.increment(machines(0), 40)

    println(s"gc4: $gc4, total: ${gc4.total}")

    Random.setSeed(1000)

    def doIt(gc: GCounter, n: Int): GCounter = {
      if (n == 0) {
        gc
      } else {
        val mach = machines(Random.nextInt(machines.length))
        val amount = Random.nextInt(10)

        doIt(gc.increment(mach, amount), n - 1)
      }
    }

    val gc5 = doIt(gc0, 100)
    val gc6 = doIt(gc1, 100)
    val gc7 = gc5 merge gc6

    println(s"gc5: $gc5, total: ${gc5.total}")
    println(s"gc6: $gc6, total: ${gc6.total}")
    println(s"gc7: $gc7, total: ${gc7.total}")
  }

  object GeneralizedCounter {
    import cats.Monoid
    import cats.instances.list._
    import cats.instances.map._
    import cats.syntax.foldable._
    import cats.syntax.semigroup._

    trait BoundedSemiLattice[A] extends Monoid[A] {
      override def combine(x: A, y: A): A
      override def empty: A
    }

    implicit val intLattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      override def combine(x: Int, y: Int): Int = x max y
      override def empty: Int = 0
    }

    implicit def setLattice[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
      override def combine(x: Set[A], y: Set[A]): Set[A] = x union y
      override def empty: Set[A] = Set.empty[A]
    }

    final case class GCounter[A](counters: Map[String,A]) {
      def increment(machine: String, amount: A)(implicit m: Monoid[A]): GCounter[A] = {
        val value = amount |+| this.counters.getOrElse(machine, m.empty)
        GCounter(counters + (machine -> value))
      }
      def merge(that: GCounter[A])(implicit b: BoundedSemiLattice[A]): GCounter[A] = GCounter(this.counters |+| that.counters)
      def total(implicit m: Monoid[A]): A = counters.values.toList.combineAll
    }
  }

  object GCounterTypeClass {
    import cats.Monoid
    import cats.instances.map._
    import cats.instances.list._
    import cats.syntax.semigroup._
    import cats.syntax.foldable._

    import GeneralizedCounter.BoundedSemiLattice

    trait GCounter[F[_, _], K, V] {
      def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]
      def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V]
      def total(f: F[K, V])(implicit m: Monoid[V]): V
    }

    object GCounter {
      def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] = counter
    }

    implicit def mapGCounterInstance[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {
      override def increment(f: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] = {
        val value = v |+| f.getOrElse(k, m.empty)
        f + (k -> value)
      }
      override def merge(f1: Map[K, V], f2: Map[K, V])(implicit b: BoundedSemiLattice[V]): Map[K, V] = f1 |+| f2
      override def total(f: Map[K, V])(implicit m: Monoid[V]): V = f.values.toList.combineAll
    }

    import cats.instances.int._

    val g1 = Map("a" -> 7, "b" -> 3)
    val g2 = Map("a" -> 2, "b" -> 5)

    val counter: GCounter[Map, String, Int] = GCounter[Map, String, Int]

    val g3     = counter.increment(g1)("c", 3)
    val g4     = counter.increment(g2)("b", 1)
    val merged = counter.merge(g3, g4)
    val total  = counter.total(merged)

    println(s"g1: $g1, g3: $g3")
    println(s"g2: $g2, g4: $g4")
    println(s"merged: $merged - total: $total")

    trait KeyValueStore[F[_, _]] {
      def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]
      def get[K, V](f: F[K, V])(k: K): Option[V]

      def getOrElse[K, V](f: F[K, V])(k: K, default: V): V = get(f)(k).fold(default)(identity)
      def values[V](f: F[_, V]): List[V]
    }

    object KeyValueStore {
      implicit val mapKVSInstance: KeyValueStore[Map] = new KeyValueStore[Map] {
        override def put[K, V](f: Map[K, V])(k: K, v: V): Map[K, V] = f + (k -> v)
        override def get[K, V](f: Map[K, V])(k: K): Option[V] = f.get(k)

        override def values[V](f: Map[_, V]): List[V] = f.values.toList
      }
    }

    implicit class KvsOps[F[_, _], K, V](f: F[K, V]) {
      def put(k: K, v: V)(implicit kvs: KeyValueStore[F]): F[K, V] = kvs.put(f)(k, v)
      def get(k: K)(implicit kvs: KeyValueStore[F]): Option[V] = kvs.get(f)(k)
      def getOrElse(k: K, default: V)(implicit kvs: KeyValueStore[F]): V = kvs.getOrElse(f)(k, default)
      def values(implicit kvs: KeyValueStore[F]): List[V] = kvs.values(f)
    }

    implicit def gcounterInstance[F[_, _], K, V](implicit kvs: KeyValueStore[F], m: Monoid[F[K, V]]) = new GCounter[F, K, V] {
      override def increment(f: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V] = {
        val value = v |+| f.getOrElse(k, m.empty)
        f.put(k, value)
      }

      override def merge(f1: F[K, V], f2: F[K, V])(implicit b: BoundedSemiLattice[V]): F[K, V] = f1 |+| f2

      override def total(f: F[K, V])(implicit m: Monoid[V]): V = f.values.combineAll
    }
  }

  StartUse
  GCounterTypeClass
}
