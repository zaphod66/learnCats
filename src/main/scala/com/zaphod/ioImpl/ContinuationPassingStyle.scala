package com.zaphod.ioImpl

object ContinuationPassingStyle {
  def add(a: Int, b: Int)(rest: Int => Unit): Unit = {
    val res = a + b
    rest(res)
  }

  def twice(a: Int)(rest: Int => Unit): Unit = {
    val res = a * 2
    rest(res)
  }

  def computation(f: Int => Unit): Unit = add(1, 2) { sum =>
    twice(sum) { res =>
      f(res)
    }
  }

  def call(): Unit = computation(res => println(s"res: $res"))
}
