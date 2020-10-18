package com.zaphod.effect

object ResourceTest extends App {

  object One {
    import cats.effect.{IO, Resource}
    import cats.implicits._

    def mkResourse(s: String) = {
      val acquire = IO(println(s"Acquiring $s")) *> IO.pure(s)
      def release(s: String) = IO(println(s"Releasing $s"))

      Resource.make(acquire)(release)
    }

    val r = for {
      outer <- mkResourse("outer")
      inner <- mkResourse("inner")
    } yield (outer, inner)

    r.use { case (a, b) => IO(println(s"Using $a and $b")) }.unsafeRunSync
  }

  object CopyFile {

  }

  One


}
