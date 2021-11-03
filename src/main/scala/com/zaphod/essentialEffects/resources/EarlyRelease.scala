package com.zaphod.essentialEffects.resources

import cats.effect.{ExitCode, IO, IOApp, Resource}
import com.zaphod.util.Debug.DebugHelper

import scala.io.Source

case class Config(connectionURL: String)

object Config {
  def fromSource(source: Source): IO[Config] =
    for {
      config <- IO(Config(source.getLines().next()))
      _      <- IO(s"read $config").debug
    } yield config
}

trait DbConnection {
  def query(sql: String): IO[String]
}

object DbConnection {
  def make(connectURL: String): Resource[IO, DbConnection] =
    Resource.make(
      IO(s"> opening Connection to $connectURL").debug *>
        IO(new DbConnection {
          override def query(sql: String): IO[String] =
            IO(s"""(results for SQL "$sql")""")
        })
    )(_ => IO(s"< closing Connection to $connectURL").debug.void)
}

object EarlyRelease extends IOApp {
  override def run(args: List[String]): IO[ExitCode] =
    dbConnectionResource.use { conn =>
      conn.query("SELECT * FROM users").debug
    }.as(ExitCode.Success)

  val sourceResource: Resource[IO, Source] =
    Resource.make(
      IO(s"> opening Source to config").debug *>
        IO(Source.fromString(config))
    )(source => IO(s"< closing Source to config").debug *>
      IO(source.close())
    )

  val configResource: Resource[IO, Config] =
//    for {
//      source <- sourceResource
//      config <- Resource.liftK(Config.fromSource(source))
//    } yield config
    Resource.liftK(sourceResource.use(Config.fromSource))
  
  val dbConnectionResource: Resource[IO, DbConnection] =
    for {
      config <- configResource
      conn   <- DbConnection.make(config.connectionURL)
    } yield conn

  val config = "exampleConnectURL"
}
