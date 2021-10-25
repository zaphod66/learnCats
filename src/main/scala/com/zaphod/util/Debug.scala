package com.zaphod.util

import cats.effect.IO

object Debug {
  implicit class DebugHelper[A](ioa: IO[A]) {
    def debug: IO[A] =
      for {
        a <- ioa
        _ = println(s"[${Thread.currentThread.getName}] $a")
      } yield a
  }
}
