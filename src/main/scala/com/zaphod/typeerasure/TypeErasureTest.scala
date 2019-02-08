package com.zaphod.typeerasure

import scala.language.higherKinds

object TypeErasureTest extends App {

  case class Thing[T](value: T)

  object Erasure {

    private def doThing(thing: Thing[_]) = {
      thing match {
        case Thing(_: Int)       => "Thing of Int"
        case Thing(_: String)    => "Thing of String"
        case Thing(_: Seq[Int]  @unchecked)  => "Thing of Seq[Int]"
        case Thing(_: Seq[Char] @unchecked) => "Thing of Seq[Char]"
        case _                   => "Thing of something"
      }
    }

    println(doThing(Thing(1)))
    println(doThing(Thing("s")))
    println(doThing(Thing(Seq(1, 2))))
    println(doThing(Thing(Seq('a', 'b'))))
    println(doThing(Thing('c')))
  }

  object ErasureTypeTag {
    import scala.reflect.runtime.universe._

    def doThing[T: TypeTag](thing: Thing[T]) = {
      typeOf[T] match {
        case t if t =:= typeOf[Int]       => "Thing of Int"
        case t if t =:= typeOf[String]    => "Thing of String"
        case t if t =:= typeOf[Seq[Int]]  => "Thing of Seq[Int]"
        case t if t =:= typeOf[Seq[Char]] => "Thing of Seq[Char]"
        case _                            => "Thing of something"
      }
    }

    println(doThing(Thing(1)))
    println(doThing(Thing("s")))
    println(doThing(Thing(Seq(1, 2))))
    println(doThing(Thing(Seq('a', 'b'))))
    println(doThing(Thing('c')))
  }

  object DuckTyping { // or structural typing
    import scala.language.reflectiveCalls

    // require that every A that is passed has a speak() method
    private def callSpeak[A <: { def speak(): String }](obj: A): Unit = {
      val s = obj.speak()

      println(s)
    }

    class Dog { def speak(): String = "woof" }
    class Cat { def speak(): String = "miao" }

    callSpeak(new Dog)
    callSpeak(new Cat)
  }

  Erasure
  ErasureTypeTag

  DuckTyping
}
