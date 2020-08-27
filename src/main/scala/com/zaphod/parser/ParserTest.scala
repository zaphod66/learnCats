package com.zaphod.parser

import scala.util.parsing.combinator._

case class WordFreq(word: String, count: Int) {
  override def toString = s"Word <$word>, number $count"
}

class SimpleParser extends RegexParsers {
  def word: Parser[String]   = """[a-z]+""".r       ^^ { _.toString }
  def number: Parser[Int]    = """([0-9]\d*)""".r ^^ { _.toInt }
  def freq: Parser[WordFreq] = word ~ number        ^^ { case wd ~ fr => WordFreq(wd,fr) }
}

object ParserTest {

  def main(args: Array[String]) = {
    val sp = new SimpleParser

    sp.parse(sp.freq, "johnny 0121") match {
      case sp.Success(matched,_) => println(matched)
      case sp.Failure(msg,_) => println(s"FAILURE: $msg")
      case sp.Error(msg,_) => println(s"ERROR: $msg")
    }
  }

}
