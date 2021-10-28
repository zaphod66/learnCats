package com.zaphod.util

object Colorize {
  def apply(a: Any): String =
    s"${colors(a.hashCode.abs % numColors)}$a${Console.RESET}"

  private val colors = List(
    Console.WHITE,
//    Console.BLACK + Console.WHITE_B,
    Console.RED,
    Console.GREEN,
    Console.YELLOW,
    Console.BLUE,
    Console.MAGENTA,
    Console.CYAN
  )

  private val numColors = colors.size - 1
}
