package com.zaphod.types

object TypesTest extends App {

  val o1 = Option(1)
  val oo1 = Option(Option(1))
  val ss1 = Some(Some(1))
  val nn2: Option[Option[Int]] = None

  type ErrorOr[A] = Either[String, A]

  val eoi1: ErrorOr[Int] = Right(1)
  val eoi2: Either[String, Int] = Right(1)

  import scala.reflect.runtime.universe._

  val o1t1 = typeTag[Option[Int]]

  val f1 = (i: Int) => i.toString

  def paramInfo[T](x: T)(implicit tag: TypeTag[T]): Unit = {
    val targs = tag.tpe match { case TypeRef(tpe, sym, args) => (tpe, sym, args) }

    println(s"Type of $x is ${tag.tpe} and has type args ${targs._3} (tpe = ${targs._1}) (sym == ${targs._2})")
  }

  println(s"o1t1 = ${o1t1.tpe}")

  paramInfo(o1)
  paramInfo(oo1)
  paramInfo(ss1)
  paramInfo(nn2)
  paramInfo(eoi1)
  paramInfo(eoi2)
  paramInfo(f1)
}
