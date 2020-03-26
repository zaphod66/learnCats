package com.zaphod.io

import cats.effect.IO

object IoTest2 extends App {
  val io1 = IO {
    println("  start io1")
    val res = 1
    println("  end   io1")

    res
  }

  val io2 = { println("before"); IO {
    println("  start io2")
    val res = 2
    println("  end   io2")

    res
  } }

  println("________________")

  val io3 = for {
    _ <- IO.pure(())
    _ = println("start io3")
    x <- io1
    y <- io2
    _ = println("end   io3")
  } yield x + y

  val res1 = io3.unsafeRunSync()

  println(s"res: $res1")

  println("________________")

  val io4 = io1.map { i =>
    println("start io4")
    val res = i + 1
    println("end   io4")

    res
  }
  val res2 = io4.unsafeRunSync()

  println(s"res: $res2")
}
