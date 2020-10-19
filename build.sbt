name := """LearnCats"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.6"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.5" % "test"

libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.14.0" % "test"

libraryDependencies += "commons-io" % "commons-io" % "2.3"

libraryDependencies += "org.typelevel" %% "cats-core" % "1.6.1"

libraryDependencies += "org.typelevel" %% "cats-free" % "1.6.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "1.4.0"

libraryDependencies += "org.typelevel" %% "cats-effect-laws" % "1.3.1" % "test"

libraryDependencies += "com.lihaoyi" %% "fastparse" % "2.2.2"

scalacOptions ++= Seq("-feature", "-deprecation", "-Ypartial-unification")

// scalacOptions += "-Ystatistics:typer"

// initialCommands in console := "import cats._, cats.implicits._"

