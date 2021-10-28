package com.zaphod.util

import cats.effect.IO

object Debug {
  implicit class DebugHelper[A](ioa: IO[A]) {
    def debug: IO[A] =
      for {
        a <- ioa
        n = Thread.currentThread.getName
        _ = println(s"[${Colorize(n)}] $a")
      } yield a
  }
}
