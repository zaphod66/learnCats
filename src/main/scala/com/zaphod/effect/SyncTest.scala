package com.zaphod.effect

import cats.laws
import cats.effect.unsafe.implicits.global

object SyncTest extends App {
  import cats.effect.{IO, Sync}

  object StackSafe {
    import cats.laws._

    private val F = Sync[IO]

    lazy val stackSafetyOnRepeatedRightBinds: laws.IsEq[IO[Unit]] = {
      val result = (0 until 10000).foldRight(F.delay(())) { (_, acc) =>
        F.delay(()).flatMap(_ => acc)
      }

      result <-> F.pure(())
    }
  }
  object Use {
    private val ioa = Sync[IO].delay(println("Hello World!"))

    ioa.unsafeRunSync()
  }

  StackSafe.stackSafetyOnRepeatedRightBinds

  Use
}
