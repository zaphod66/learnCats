name := """LearnCats"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.6"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.7")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.3"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "0.9"

libraryDependencies += "org.typelevel" %% "cats-effect-laws" % "0.9" % "test"

val scalazVersion = "7.2.21"

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % scalazVersion,
  "org.scalaz" %% "scalaz-concurrent" % scalazVersion,
  "org.scalaz" %% "scalaz-effect" % scalazVersion,
  //  "org.scalaz" %% "scalaz-typelevel" % scalazVersion,
  "org.scalaz" %% "scalaz-ioeffect" % "2.1.0",
)

scalacOptions ++= Seq("-feature", "-deprecation", "-Ypartial-unification")

// scalacOptions += "-Ystatistics:typer"

// initialCommands in console := "import cats._, cats.implicits._"

