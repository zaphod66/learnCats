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
}
