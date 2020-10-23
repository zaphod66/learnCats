package com.zaphod.stuff

import scala.annotation.tailrec


object Expr {
  sealed trait Term
  case class Num(value: Double) extends Term
  case class Var(value: Char) extends Term
  case class Add(a1: Term, a2: Term) extends Term
  case class Mul(m1: Term, m2: Term) extends Term
  case class Pow(v: Var, n: Num) extends Term
  case class Sin(t: Term) extends Term
  case class Cos(t: Term) extends Term

  def makeAdd(a1: Term, a2: Term): Term = (a1, a2) match {
    case (Num(0), _)      => a2
    case (_, Num(0))      => a1
    case (Num(x), Num(y)) => Num(x + y)
    case _                => Add(a1, a2)
  }

  def makeMul(m1: Term, m2: Term): Term = (m1, m2) match {
    case (Num(0), _)      => Num(0)
    case (Num(1), _)      => m2
    case (_, Num(0))      => Num(0)
    case (_, Num(1))      => m1
    case (Num(x), Num(y)) => Num(x * y)
    case _                => Mul(m1, m2)
  }

  def makePow(v: Var, n: Num): Term = n match {
    case Num(0) => Num(1)
    case Num(1) => v
    case _ => Pow(v, n)
  }

  import cats.Monoid

  val mulMonoid: Monoid[Term] = new Monoid[Term] {
    override def empty: Term = Num(1.0)
    override def combine(x: Term, y: Term): Term = makeMul(x, y)
  }

  val addMonoid: Monoid[Term] = new Monoid[Term] {
    override def empty: Term = Num(0.0)
    override def combine(x: Term, y: Term): Term = makeAdd(x, y)
  }
}

object TermShow {
  import Expr._
  import cats.Show
  import cats.syntax.show._

  implicit val showNum: Show[Num] = Show.show { num =>
    val i = num.value.toInt
    if (i == num.value)
      i.toString
    else
      num.value.toString
  }
  implicit val showVar: Show[Var] = Show.show(v => v.value.toString)
  implicit val showAdd: Show[Add] = Show.show(add => s"${add.a1.show} + ${add.a2.show}")
  implicit val showMul: Show[Mul] = Show.show(mul => s"${mul.m1.show} * ${mul.m2.show}")
  implicit val showPow: Show[Pow] = Show.show(pow => s"${pow.v.show}^${pow.n.show}")
  implicit val showSin: Show[Sin] = Show.show(sin => s"sin(${sin.t.show})")
  implicit val showCos: Show[Cos] = Show.show(cos => s"cos(${cos.t.show})")

  implicit val showTerm: Show[Term] = Show.show {
    case n: Num => n.show
    case v: Var => v.show
    case a: Add => a.show
    case m: Mul => m.show
    case s: Sin => s.show
    case c: Cos => c.show
    case p: Pow => p.show
  }
}

object Derive {
  import Expr._

  def derive(term: Term): Term = {
    term match {
      case Num(_) => Num(0)
      case Var(_) => Num(1)
      case Add(a1, a2) => makeAdd(derive(a1), derive(a2))
      case Mul(m1, m2) => makeAdd(makeMul(derive(m1), m2), makeMul(m1, derive(m2)))
      case Sin(t) => makeMul(derive(t), Cos(t))
      case Cos(t) => makeMul(derive(t), Mul(Num(-1), Sin(t)))
      case Pow(v, n) => makeMul(n, makePow(v, Num(n.value-1)))
    }
  }
}

object Parser {
  import Expr._
  import fastparse._, NoWhitespace._

  private def space[_: P]         = P( CharsWhileIn(" \r\n", 0) )
  private def digits[_: P]        = P( CharsWhileIn("0-9") )
  private def exponent[_: P]      = P( CharIn("eE") ~ CharIn("+\\-").? ~ digits )
  private def fractional[_: P]    = P( "." ~ digits )
  private def integral[_: P]      = P( "0" | CharIn("1-9")  ~ digits.? )

  private def num[_: P]: P[Num] = P(  CharIn("+\\-").? ~ integral ~ fractional.? ~ exponent.? ).!.map(
    x => Num(x.toDouble)
  )
  private def vaz[_: P]: P[Var] = P( space.? ~ CharIn("a-z").! ).map( v => Var(v(0)) )
  private def par[_: P]: P[Term] = P( "(" ~ add ~ ")" )
  private def pow[_: P]: P[Term] = P( vaz ~ "^" ~ num ).map( e => Pow(e._1, e._2))
  private def fac[_: P]: P[Term] = P( par | pow | vaz | num )
  private def mul1[_: P]: P[Term] = P( fac ~ space.? ~/ ("*" ~ space.? ~/ fac).? ).map {
    case (t1, Some(t2)) => makeMul(t1, t2)
    case (t1, None) => t1
  }
  private def mul[_: P]: P[Term] = P( fac ~ space.? ~/ ("*" ~ space.? ~/ fac).rep ).map { s=>
    s._2.fold(s._1)((t1, t2) => makeMul(t1, t2))
  }

  private def add1[_: P]: P[Term] = P( mul ~ space.? ~/ ("+" ~ space.? ~/ mul).? ).map {
    case (t1, Some(t2)) => makeAdd(t1, t2)
    case (t1, None) => t1
  }

  private def add[_: P]: P[Term] = P( mul ~ space.? ~/ ("+" ~ space.? ~/ mul).rep ).map { s =>
    s._2.toList.fold(s._1)((t1, t2) => makeAdd(t1, t2))
  }

  private def ter[_: P]: P[Term] = P( add ~ End )

  def parse(str: String): Either[String, Term] = fastparse.parse(str, ter(_)) match {
    case Parsed.Success(t, _)    => Right(t)
    case Parsed.Failure(_, _, e) => Left(e.trace().longAggregateMsg)
  }
}

object FunctionDerivation extends App {
  import Expr._

  val par1 = Add(Pow(Var('x'), Num(2)), Num(1))   // x^2 + 1
  val par2 = Add(Mul(Num(2.5), Pow(Var('x'), Num(3))), Add(Mul(Num(1.5), Pow(Var('x'), Num(2))), Num(1))) // 2.5*x^3 + 1.5*x^2 + 1
  val sin1 = Sin(Var('x'))
  val sinc = Mul(Var('x'), Sin(Var('x')))
  val root = Pow(Var('x'), Num(0.5))

  import TermShow._
  import cats.syntax.show._
  import Derive.derive

  println(s"$par1 => ${par1.show}")
  println(s"${par1.show}' => ${derive(par1).show}")
  println(s"${par1.show}'' => ${derive(derive(par1)).show}")

  println(s"${par2.show}'   => ${derive(par2).show}")
  println(s"${par2.show}''  => ${derive(derive(par2)).show}")
  println(s"${par2.show}''' => ${derive(derive(derive(par2))).show}")

  println(s"${sin1.show}' => ${derive(sin1).show}")
  println(s"${sin1.show}'' => ${derive(derive(sin1)).show}")
  println(s"${sin1.show}''' => ${derive(derive(derive(sin1))).show}")
  println(s"${sin1.show}'''' => ${derive(derive(derive(derive(sin1)))).show}")

  println(s"${sinc.show}' => ${derive(sinc).show}")
  println(s"${root.show}' => ${derive(root).show}")
}

object StringToTerm extends App {
  import Derive.derive

  val str = "-1.5 * x^3 + x^2 + x + 2"
//  val str = "-1.5 * x^3 + 2.5 * x^2 + 1.5 * x^1"  //  fails
//  val str = "-1.5 * x^3 + 2.5 * x^2"
  val res = Parser.parse(str)

  import Expr.Term

  private def pp(termM: Either[String, Term]): String = {
    import TermShow._
    import cats.syntax.show._

    termM.fold(
      e => s"Error: $e",
      t => t.show
    )
  }

  @tailrec
  def der(t: Term, n: Int): Term = { if (n <= 0) t else der(derive(t), n - 1) }

  println(s"str: $str")
  (0 to 3) foreach( i => println(s"de$i: ${pp(res.map(der(_, i)))}"))
}
