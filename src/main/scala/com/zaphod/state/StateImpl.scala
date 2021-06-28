package com.zaphod.state

object StateImpl extends App {
  case class State[S, A](run: S => (S, A)) {
    def map[B](f: A => B): State[S, B] = State(s1 => {
      val (s2, a) = run(s1)
      (s2, f(a))
    })

    def flatMap[B](f: A => State[S, B]): State[S, B] = State(s1 => {
      val (s2, a) = run(s1)
      f(a).run(s2)
    })
  }

  object State {
    def pure[S, A](a: => A): State[S, A]     = State(s => (s, a))

    def getState[S]: State[S, S]             = State(s => (s, s))
    def setState[S](s: => S): State[S, Unit] = State(_ => (s, ()))
  }
  
  // ----------------------

  def zipWithIndex[A](as: List[A]): List[(Int, A)] = {
    import com.zaphod.state.StateImpl.State._

    as.foldLeft(State.pure[Int, List[(Int, A)]](List.empty)) { (acc, a) =>
      for {
        xs <- acc
        n  <- getState
        _  <- setState(n + 1)
      } yield (n, a) :: xs }.run(0)._2.reverse
  }

  val list1 = List(4, 3, 2, 1)
  val list2 = zipWithIndex(list1)
  val list3 = List('a', 'b', 'c', 'd')
  val list4 = zipWithIndex(list3)
  val list5 = List.fill(10)(1)
  val list6 = zipWithIndex(list5)

  println(s"list1 = $list1")
  println(s"list2 = $list2")
  println(s"list3 = $list3")
  println(s"list4 = $list4")
  println(s"list5 = $list5")
  println(s"list6 = $list6")

  sealed trait Tree[A]
  final case class Leaf[A](a: A) extends Tree[A]
  final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

  def labelPlain[A](t: Tree[A])(seed: Int): (Tree[(A,Int)], Int) = t match {
    case Leaf(x)      => (Leaf(x, seed), seed + 1)
    case Branch(l, r) => labelPlain(l)(seed) match {
      case (ll, ls) => {
        labelPlain(r)(ls) match {
          case (rr, rs) => (Branch(ll, rr), rs)
        }
      }
    }
  }

  import com.zaphod.state.StateImpl.State._

  def labelState[A](t: Tree[A]): State[Int, Tree[(A, Int)]] = t match {
    case Leaf(x) => for {
                      n <- getState
                      _ <- setState(n + 1)
                    } yield Leaf((x, n))
    case Branch(l, r) => for {
                            ll <- labelState(l)
                            rr <- labelState(r)
                         } yield Branch(ll, rr)
  }

  val tree1: Tree[String] = Branch(Branch(Leaf("one"), Branch(Leaf("two"), Leaf("three"))), Leaf("four"))
  val tree2 = labelPlain(tree1)(1)._1
  val tree3 = labelState(tree1).run(1)._2

  println(s"tree1: $tree1")
  println(s"tree2: $tree2")
  println(s"tree3: $tree3")
}
