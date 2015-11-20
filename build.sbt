name := """LearnCats"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.3"

libraryDependencies += "org.spire-math" %% "cats" % "0.2.0"

scalacOptions += "-feature"

initialCommands in console := "import cats._, cats.std.all._, cats.implicits._"

