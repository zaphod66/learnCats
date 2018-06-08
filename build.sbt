name := """LearnCats"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.6"

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.3"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "0.9"

libraryDependencies += "org.typelevel" %% "cats-effect-laws" % "0.9" % "test"

scalacOptions ++= Seq("-feature", "-Ypartial-unification", "-Ystatistics:typer")

// initialCommands in console := "import cats._, cats.implicits._"

