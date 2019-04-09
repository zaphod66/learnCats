// https://eli-jordan.github.io/2018/02/16/life-is-a-comonad/

package com.zaphod.comonad

object ComonadTest extends App {

  object Zipper {
    import cats.Comonad

    case class StreamZipper[A](left: Stream[A], focus: A, right: Stream[A]) {
      def moveLeft: StreamZipper[A]  = new StreamZipper[A](left.tail, left.head, focus #:: right)
      def moveRight: StreamZipper[A] = new StreamZipper[A](focus #:: left, right.head, right.tail)

      // A stream of zippers, with focus set to each element on the left
      private lazy val lefts: Stream[StreamZipper[A]]  = Stream.iterate(moveLeft)(_.moveLeft).zip(left.tail).map(_._1)

      // A stream of zippers, witgh focus set to each element on the right
      private lazy val rights: Stream[StreamZipper[A]] = Stream.iterate(moveRight)(_.moveRight).zip(right.tail).map(_._1)

      lazy val coflatten: StreamZipper[StreamZipper[A]] = new StreamZipper[StreamZipper[A]](lefts, this, rights)

      def toList: List[A] = left.toList.reverse ++ List(focus) ++ right.toList
    }

    object ZipperMonad extends Comonad[StreamZipper] {
      override def map[A, B](fa: StreamZipper[A])(f: A => B): StreamZipper[B] = new StreamZipper[B](fa.left.map(f), f(fa.focus), fa.right.map(f))

      override def extract[A](fa: StreamZipper[A]): A = fa.focus

      override def coflatten[A](fa: StreamZipper[A]): StreamZipper[StreamZipper[A]] = fa.coflatten
      override def coflatMap[A, B](fa: StreamZipper[A])(f: StreamZipper[A] => B): StreamZipper[B] = map(coflatten(fa))(f)
    }

    def avg(a: StreamZipper[Int]): Double = {
      val left  = a.moveLeft.focus
      val focus = a.focus
      val right = a.moveRight.focus

      println(s"avg($left, $focus, $right)")

      (left + focus + right) / 3.0d
    }

    val s1 = StreamZipper(List(3, 2, 1).toStream, 4, List(5, 6, 7).toStream)

    val av = ZipperMonad.coflatMap(s1)(avg)

    println(s"s1: $s1 -> ${s1.toList}")
    println(s"av: $av -> ${av.toList}")
  }

  object GameOfLife {
    import scala.language.higherKinds
    import cats.{Comonad, Functor}
    import cats.syntax.functor._

    //    cojoin>
//    - Since we are fixing S we want to generate Store[S, Store[S, A]].
//    - Since we are replacing A with Store[S, A], and A is only used in the return type of lookup, we need to define a new lookup function of type S => Store[S, A].
//      - We do this by partially applying the store constructor Store(lookup) since this has exactly the type we need.
//    - We then copy the current store, replacing the lookup function with a partially applied constructor and we’re done.

    case class Store[S, A](lookup: S => A)(val index: S) {
      lazy val counit: A = lookup(index)
      lazy val coflatten: Store[S, Store[S, A]] = Store(Store(lookup))(index)
      def coflatMap[B](f: Grid[A] => B): Store[S, B] = map(coflatten(this))(f)
      def map[B](f: A => B): Store[S, B] = Store(lookup andThen f)(index)
      def experiment[F[_] : Functor](fn: S => F[S]): F[A] = fn(index).map(lookup)
    }

    type Coord   = (Int, Int)
    type Grid[A] = Store[Coord, A]

    def conway(grid: Grid[Boolean]): Boolean = {
      def neighbourCoords(x: Int, y: Int): List[Coord] = List(
        (x - 1, y - 1),
        (x - 1, y    ),
        (x - 1, y + 1),
        (x    , y - 1),
        (x    , y + 1),
        (x + 1, y - 1),
        (x + 1, y    ),
        (x + 1, y + 1)
      )

      val neighbours = grid.experiment[List] { case (x, y) => neighbourCoords(x, y) }
      val liveCount  = neighbours.count(identity)

      grid.counit match {
        case true  if liveCount <  2                   => false
        case true  if liveCount == 2 || liveCount == 3 => true
        case true  if liveCount >  3                   => false
        case false if liveCount == 3                   => true
        case v => v
      }
    }

    def step(grid: Grid[Boolean]): Grid[Boolean] = grid.coflatMap(conway)
  }

  Zipper

  GameOfLife
}
