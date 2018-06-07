// package com.zaphod.monadFree
/*
import cats.free.Free
import cats.{Id, ~>}

object Logo {
  case class Position(x: Double, y: Double, heading: Degree)
  case class Degree(private val d: Int) {
    val value = d % 360
  }

  sealed trait Instruction[A]

  case class Forward(pos: Position, steps: Int) extends Instruction[Position]
  case class Backward(pos: Position, steps: Int) extends Instruction[Position]
  case class RotateLeft(pos: Position, deg: Degree) extends Instruction[Position]
  case class RotateRight(pos: Position, deg: Degree) extends Instruction[Position]

  case class ShowPosition(position: Position) extends Instruction[Unit]

  def forward(pos: Position, steps: Int): Free[Instruction, Position] = Free.liftF(Forward(pos, steps))
  def backward(pos: Position, steps: Int): Free[Instruction, Position] = Free.liftF(Backward(pos, steps))
  def left(pos: Position, degree: Degree): Free[Instruction, Position] = Free.liftF(RotateLeft(pos, degree))
  def right(pos: Position, degree: Degree): Free[Instruction, Position] = Free.liftF(RotateRight(pos, degree))
  def showPosition(pos: Position): Free[Instruction, Unit] = Free.liftF(ShowPosition(pos))
}

object Computations {
  import Logo._

  private def Deg2Rad(d: Double): Double = d / 180.0 * math.Pi
  private def dx(steps: Int, d: Degree) = { val dd = steps * math.sin(Deg2Rad(d.value)); println(s"steps: $steps d: $d => dx = $dd"); dd }
  private def dy(steps: Int, d: Degree) = { val dd = steps * math.cos(Deg2Rad(d.value)); println(s"steps: $steps d: $d => dy = $dd"); dd }

  def forward(p: Position, steps: Int) = Position(p.x + dx(steps, p.heading), p.y + dy(steps, p.heading), p.heading)
  def backward(p: Position, steps: Int) = forward(p, -steps)
  def left(p: Position, d: Degree) = Position(p.x, p.y, Degree(p.heading.value - d.value))
  def right(p: Position, d: Degree) = Position(p.x, p.y, Degree(p.heading.value + d.value))
  def show(p: Position): Unit = println(p)
}


object MonadFree extends App {
  import Logo._

  val program1: Position => Free[Instruction, Position] = {
    start => for {
      p1 <- forward(start, 10)
      p2 <- right(p1, Degree(90))
      p3 <- forward(p2, 10)
    } yield p3
  }

  val program2: Position => Free[Instruction, Position] = {
    start => for {
      p1 <- forward(start, 10)
      p2 <- right(p1, Degree(90))
      p3 <- forward(p2, 10)
      p4 <- right(p3, Degree(90))
      p5 <- forward(p4, 20)
      p6 <- right(p5, Degree(90))
      p7 <- forward(p6, 10)
      p8 <- right(p7, Degree(90))
      p9 <- forward(p8, 10)
    } yield p9
  }

  object InterpreterId extends (Instruction ~> Id) {

    override def apply[A](fa: Instruction[A]): Id[A] = {
      val pp = fa match {
        case Forward(p, l)     => Computations.forward(p, l)
        case Backward(p, l)    => Computations.backward(p, l)
        case RotateLeft(p, d)  => Computations.left(p, d)
        case RotateRight(p, d) => Computations.right(p, d)
        case ShowPosition(p)   => Computations.show(p)
      }

      pp.asInstanceOf[Id[A]]
    }
  }


  object InterpreterOption extends (Instruction ~> Option) {

    val nonNegative: (Position) => Option[Position] = {
      p => if (p.x >= 0 &&p.y >= 0) Some(p) else None
    }

    override def apply[A](fa: Instruction[A]) = {
      val pp = fa match {
        case Forward(p, l)     => nonNegative(Computations.forward(p, l))
        case Backward(p, l)    => nonNegative(Computations.backward(p, l))
        case RotateLeft(p, d)  => Some(Computations.left(p, d))
        case RotateRight(p, d) => Some(Computations.right(p, d))
        case ShowPosition(p)   => Some(Computations.show(p))
      }

      pp.asInstanceOf[Option[A]]
    }
  }

  import cats._
  import cats.std.all._

  val startPosition = Position(0.0, 0.0, Degree(0))
  val result11 = program1(startPosition).foldMap(InterpreterId)
  val result21 = program1(startPosition).foldMap(InterpreterOption)
  val result12 = program2(startPosition).foldMap(InterpreterId)
  val result22 = program2(startPosition).foldMap(InterpreterOption)

  println("prog1")
  println(result11)
  println(result21)

  println("prog2")
  println(result12)
  println(result22)
}
*/