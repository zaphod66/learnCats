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

  def typeInfo[T](x: T)(implicit tag: TypeTag[T]): (T, Type, Symbol, List[Type]) = {
    tag.tpe match { case TypeRef(tpe, sym, args) => (x, tpe, sym, args) }
  }

  def printTypeInfo[T](info: (T, Type, Symbol, List[Type]))(implicit tag: TypeTag[T]): Unit = {
    println(s"Type of ${info._1} is ${tag.tpe} and has type args ${info._4} (tpe = ${info._2}) (sym = ${info._3})")
  }

  println(s"o1t1 = ${o1t1.tpe}")

  printTypeInfo(typeInfo(o1))
  printTypeInfo(typeInfo(oo1))
  printTypeInfo(typeInfo(ss1))
  printTypeInfo(typeInfo(nn2))
  printTypeInfo(typeInfo(eoi1))
  printTypeInfo(typeInfo(eoi2))
  printTypeInfo(typeInfo(f1))
}
