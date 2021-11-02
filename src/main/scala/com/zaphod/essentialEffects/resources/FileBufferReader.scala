package com.zaphod.essentialEffects.resources

import cats.effect.{ExitCode, IO, IOApp, Resource}
import cats.implicits.catsSyntaxApplicativeError
import com.zaphod.util.Debug.DebugHelper

import java.io.RandomAccessFile

class FileBufferReader private (in: RandomAccessFile) {
  def readBuffer(offset: Long): IO[(Array[Byte], Int)] =
    IO {
      in.seek(offset)

      val buf = new Array[Byte](FileBufferReader.bufferSize)
      val len = in.read(buf)

      (buf, len)
    }

  private def close: IO[Unit] = IO(in.close())
}

object FileBufferReader {
  val bufferSize = 4096

  def makeResource(fileName: String): Resource[IO, FileBufferReader] =
    Resource.make {
      IO(new FileBufferReader(new RandomAccessFile(fileName, "r")))
    } { res =>
      res.close
    }
}

object FileBufferReaderApp extends IOApp {
  override def run(args: List[String]): IO[ExitCode] = {
    IO(System.getProperty("user.dir")).debug *>
    FileBufferReader.makeResource("build.sbt").use { fbr =>
      for {
        (_, len) <- fbr.readBuffer(0L)
      } yield s"read $len bytes"
    }.recover(_.getMessage)
      .debug
      .as(ExitCode.Success)
  }
}
