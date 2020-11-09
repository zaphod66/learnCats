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

object CatsParser {
  import Expr._

  import cats.implicits.toBifunctorOps

  import cats.parse.{Parser => P, Parser1, Numbers}

  private[this] val whitespace: Parser1[Unit] = P.charIn(" \t\r\n").void
  private[this] val whitespaces0: P[Unit] = whitespace.rep.void

  private def num: Parser1[Num]  = (whitespaces0.with1 *> Numbers.jsonNumber <* whitespaces0).map(x => Num(x.toDouble))
  private def vaz: Parser1[Var]  = (whitespaces0.with1 *> P.charIn("xyz") <* whitespaces0).map(Var)
  private def pow: Parser1[Term] = whitespaces0.with1 *> ((vaz <* P.char('^')) ~ num).map(vn => makePow(vn._1, vn._2)) <* whitespaces0
  private def fac: Parser1[Term] = whitespaces0.with1 *> P.oneOf1(List(P.backtrack1(pow), vaz, num)) <* whitespaces0

  private def mul = P.rep1Sep(fac, 1, P.char('*')).map { terms =>
    terms.foldLeft[Term](Num(1.0)) {
      case (acc, term) => makeMul(acc, term)
    }
  }

  private def add = P.rep1Sep(mul, 1, P.char('+')).map { terms =>
    terms.foldLeft[Term](Num(0.0)) {
      case (acc, term) => makeAdd(acc, term)
    }
  }

  private val parser = P.oneOf1( List(add) )
  private def ter: Parser1[Term] = whitespaces0.with1 *> parser <* (whitespaces0 ~ P.end)

  def parse(str: String): Either[String, Term] = ter.parse(str)
    .leftMap(e => e.toString).map(_._2)
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

  val funcs = List(
    "2.7182818",
    "1 * x^1 + 3 * x^4 + 5 * x^6",
    "1 * x^3 + 1 * x + 4 * x^2 + 1 * x",
    "x^5 + x^4 + x^3 + x^2 + x^1 + 1",
    "2 * x^3 + x^2 + x + 2",
    "2 * x^3 + 2 * x^2 + x + 2",
    "-1.5 * x^3 + 2.5 * x^2 + 1.5 * x^1",
    "-1.5 * x^3 + 2.5 * x^2"
  )

  val str = "-1.5 * x^3 + 2.5 * x^2 + 1.5 * x^1"
  val res = CatsParser.parse(str)

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

//  println(s"str: $str")
//  (0 to 3) foreach( i => println(s"catsParse de$i: ${pp(res.map(der(_, i)))}"))

  val ress = funcs map CatsParser.parse

  funcs foreach { func =>
    val res = CatsParser.parse(func)

    println(s"str: $func")
    (0 to 3) foreach( i => println(s"catsParse de$i: ${pp(res.map(der(_, i)))}"))
    println("---")
  }
}