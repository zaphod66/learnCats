package com.zaphod.bracket

import java.io.{File, FileReader}

import cats.effect.{Blocker, ContextShift, ExitCode, IO, IOApp}

object BracketTest extends IOApp{

  import cats.syntax.functor._
  import cats.syntax.apply._

  val file = new File("src/main/resources/example.txt")

  override def run(args: List[String]): IO[ExitCode] = {
    val line: IO[String] = Blocker[IO].use { blocker =>
      import Files._

//      open(file)(blocker).flatMap { reader =>
//        read(blocker)(reader) <* close(blocker)(reader)
//      }

      open(file)(blocker).bracket { read(blocker) } { close(blocker) }
    }

    line.flatMap { line =>
      IO { println(line) }
    }.as(ExitCode.Success)
  }
}

object Files {
  def open(file: File)(blocker: Blocker)(implicit cs: ContextShift[IO]): IO[FileReader] = blocker.delay[IO, FileReader] {
    println("open FileReader")
    new FileReader(file)
  }

  def read(blocker: Blocker)(reader: FileReader)(implicit cs: ContextShift[IO]): IO[String] = blocker.delay[IO, String] {
    val buffer = Array.fill[Char](4096)(0)
    reader.read(buffer)
    new String(buffer).trim
  }

  def close(blocker: Blocker)(reader: FileReader)(implicit cs: ContextShift[IO]): IO[Unit] = blocker.delay[IO, Unit] {
    println("close FileReader")
    reader.close()
  }
}
