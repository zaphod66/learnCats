package com.zaphod.essentialEffects.tryouts

import cats.effect.{ExitCode, IO, IOApp}
import cats.implicits.catsSyntaxParallelSequence1

import com.zaphod.util.Debug.DebugHelper

import java.awt.Color
import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO

object ConvolveImage extends IOApp {
  val fileName = "/Users/nscheller/Downloads/NilsStundenplan_2021_08.jpeg"
  val destName = "/Users/nscheller/Downloads/AAA.png"
  val srcImage: BufferedImage = ImageIO.read(new File(fileName))
  val resImage = new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_INT_ARGB)

  override def run(args: List[String]): IO[ExitCode] =
    for {
      _   <- IO(withTS(s"reading $fileName")).debug
      src <- IO(ImageIO.read(new File(fileName)))
      _   <- IO(withTS(s"creating destImage")).debug
      res <- IO(new BufferedImage(srcImage.getWidth, srcImage.getHeight, BufferedImage.TYPE_INT_ARGB))
      _   <- IO(withTS(s"manipulating halfRed")).debug
      _   <- manipulateImage(src, res)(halfRed)
      _   <- IO(withTS(s"blurring")).debug
      _   <- manipulateImage(res, res)(blur)
      _   <- IO(withTS(s"writing $destName")).debug
      _   <- IO(ImageIO.write(res, "PNG", new File(destName)))
    } yield ExitCode.Success

  def withTS(s: String): String =
    s"$s - ${System.currentTimeMillis()}"

  def manipulateImage(src: BufferedImage, dest: BufferedImage)(f: (Int, Int, BufferedImage, BufferedImage) => Unit): IO[BufferedImage] = for {
    _   <- (for (x <- 0 until src.getWidth; y <- 0 until src.getHeight) yield IO(f(x, y, src, dest))).toList.parSequence
  } yield dest

  def halfRed(x: Int, y: Int, srcImage: BufferedImage, resImage: BufferedImage): Unit = {
    val pixel = new Color(srcImage.getRGB(x, y))
    val color = new Color((pixel.getRed / 1.2).toInt, pixel.getGreen, pixel.getBlue, pixel.getAlpha)

    resImage.setRGB(x, y, color.getRGB)
  }

  def halfGreen(x: Int, y: Int, srcImage: BufferedImage, resImage: BufferedImage): Unit = {
    val pixel = new Color(srcImage.getRGB(x, y))
    val color = new Color(pixel.getRed, (pixel.getGreen / 1.4).toInt, pixel.getBlue, pixel.getAlpha)

    resImage.setRGB(x, y, color.getRGB)
  }

  def halfBlue(x: Int, y: Int, srcImage: BufferedImage, resImage: BufferedImage): Unit = {
    val pixel = new Color(srcImage.getRGB(x, y))
    val color = new Color(pixel.getRed, pixel.getGreen, pixel.getBlue / 2, pixel.getAlpha)

    resImage.setRGB(x, y, color.getRGB)
  }

  def blur(x: Int, y: Int, srcImage: BufferedImage, resImage: BufferedImage): Unit = {
    val bx = 5
    val by = 5
    val w  = srcImage.getWidth - 1
    val h  = srcImage.getHeight - 1

    val rgbs = for {
      dx <- x - bx to x + bx
      dy <- y - by to y + by
      cx = if (dx < 0) 0 else if (dx > w) w else dx
      cy = if (dy < 0) 0 else if (dy > h) h else dy
      p  = new Color(srcImage.getRGB(cx, cy))
    } yield (p.getRed, p.getGreen, p.getBlue)

    val s = (2 * bx + 1) * (2 * by + 1)
    val r = rgbs.map(_._1).sum / s
    val g = rgbs.map(_._2).sum / s
    val b = rgbs.map(_._3).sum / s
    val c = new Color(r, g, b)

    resImage.setRGB(x, y, c.getRGB)
  }
}
