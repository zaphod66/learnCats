package com.zaphod.state

object StateTest extends App {
  sealed trait Tree[A]

  final case class Leaf[A](a: A) extends Tree[A]
  final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  def blowUp(tree: Tree[Int]): Tree[Int] = tree match {
    case Branch(l, r) => Branch(blowUp(l), blowUp(r))
    case Leaf(a)      => Branch(Leaf(a - 1), Leaf(a + 1))
  }

  @scala.annotation.tailrec
  def blowUpN(tree: Tree[Int], n: Int): Tree[Int] = {
    if (n <= 0) tree
    else blowUpN(blowUp(tree), n - 1)
  }

  val tree1: Tree[String] = Branch(Branch(Leaf("one"), Branch(Leaf("two"), Leaf("three"))), Leaf("four"))
  val tree2: Tree[Int] = Branch(Branch(Leaf(1), Branch(Leaf(2), Leaf(3))), Leaf(4))
  val tree3: Tree[Int] = blowUpN(tree2, 1)
  val tree4: Tree[Int] = blowUpN(tree2, 2)
  val tree5: Tree[Int] = blowUpN(tree2, 20)

  def labelTree[A](tree: Tree[A]): Tree[(A, Int)] = {
    import cats.data.State

    def go(tree: Tree[A]): State[Int, Tree[(A, Int)]] = State[Int, Tree[(A, Int)]] { s1 =>
      tree match {
        case Leaf(a)      => (s1 + 1, Leaf((a, s1)))
        case Branch(l, r) =>
          val (s2, ll) = go(l).run(s1).value
          val (s3, rl) = go(r).run(s2).value

          (s3, Branch(ll, rl))
      }
    }

    go(tree).run(0).value._2
  }

  println(s"tree1: $tree1")
  println(s"label: ${labelTree(tree1)}")
  println(s"tree2: $tree2")
  println(s"label: ${labelTree(tree2)}")
  println(s"tree3: $tree3")
  println(s"label: ${labelTree(tree3)}")
  println(s"tree4: $tree4")
  println(s"label: ${labelTree(tree4)}")
//  println(s"tree5: $tree5")
//  println(s"label: ${labelTree(tree5)}")

  // Pascal triangle

  def pascal(c: Int, r: Int): BigInt = {
    import cats.data.State

    def go1(c: BigInt, r: BigInt): State[Map[(BigInt,BigInt), BigInt], BigInt] = State[Map[(BigInt,BigInt), BigInt], BigInt] { s1 =>
      s1.get((c, r)) match {
        case Some(a) =>
          (s1, a)
        case None    =>
          if (c < 0 || c > r) {
            (s1, 0)
          }
          else if (r == 0) {
            (s1, 1)
          }
          else {
            val (s2, a) = go1(c, r - 1).run(s1).value
            val (s3, b) = go1(c - 1, r - 1).run(s2).value
            val p = a + b
            val s4 = s3 + ((c, r) -> p)
            (s4, p)
          }
      }
    }

    def go2(c: BigInt, r: BigInt, acc: Map[(BigInt,BigInt), BigInt]): BigInt = {
      acc.get((c, r)) match {
        case Some(a) => a
        case None    =>
          if (c < 0 || c > r) {
            0
          } else if (r == 0) {
            1
          } else {
            val a = go2(c, r - 1, acc)
            val acc2 = acc + ((c, r - 1) -> a)
            val b = go2(c - 1, r - 1, acc2)
            val acc3 = acc2 + ((c - 1, r - 1) -> b)
            val p = a + b
            val acc4 = acc3 + ((c, r) -> p)
            go2(c, r, acc4)
          }
      }
    }

//    go1(c, r).run(Map.empty).value._2
    go2(c, r, Map.empty)
  }

  (0 to 9) foreach { r =>
    (0 to r) foreach { c =>
      println(s"pascal($c, $r) = ${pascal(c, r)}")
    }
    println("--------------")
  }
}
