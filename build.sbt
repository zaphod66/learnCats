name := """LearnCats"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.11.9"

libraryDependencies += "org.scalatest" %% "scalatest" % "2.2.4" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.11.4" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.3"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.0.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "0.9"

libraryDependencies += "org.typelevel" %% "cats-effect-laws" % "0.9" % "test"

scalacOptions += "-feature"

initialCommands in console := "import cats._, cats.implicits._"

