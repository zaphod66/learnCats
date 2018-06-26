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

  StartUse
}
