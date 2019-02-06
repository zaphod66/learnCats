package com.zaphod.stuff

import cats.free.Free

object StdinTest extends App {

//val lines = io.Source.stdin.getLines

  val lines = List("10", "203 204 205 206 207 208 203 204 205 206", "13", "203 204 204 205 206 207 205 208 203 206 205 206 204")

  def processLines(lines: List[String]): (List[Int], List[Int]) = {
    val numA = lines(0).toInt
    val linA = lines(1).split(" ") map { _.toInt }
    val numB = lines(2).toInt
    val linB = lines(3).split(" ") map { _.toInt }

    (linA.toList, linB.toList)
  }

  def calcDiff(linA: List[Int], linB: List[Int]): List[Int] = {
    def go(linA: List[Int], linB: List[Int], accu: List[Int]): List[Int] = {
      println(s"go($linA, $linB, $accu)")
      println("===")
      println(linA)
      println(linB)
      println(accu)
      if (linA.isEmpty)
        accu
      else {
        val vA = linA.head
        val vB = linB.head
        if (linA.head == linB.head) {
          go(linA.tail, linB.tail, accu)
        } else {
          go(linA, linB.tail, linB.head :: accu)
        }
      }
    }

    go(linA, linB, Nil).distinct.sorted
//    go(linA, linB, List(205, 206, 204, 206)).distinct.sorted
  }

  val (linA, linB) = processLines(lines)
  val diff = calcDiff(linA, linB)

  println(diff.mkString(" "))
}
