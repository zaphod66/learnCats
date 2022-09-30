name := """LearnCats"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.13.5"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full)
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.1")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.6.1"

libraryDependencies += "org.typelevel" %% "cats-effect" % "3.2.8"
libraryDependencies += "org.typelevel" %% "cats-effect-laws" % "3.2.8"
libraryDependencies += "org.typelevel" %% "cats-effect-kernel-testkit" % "3.2.8"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

ThisBuild / scalacOptions ++= Seq("-feature", "-deprecation")
// ThisBuild / scalacOptions += "-P:semanticdb:synthetics:on"
// ThisBuild / scalacOptions += "-Ystatistics:typer"

// initialCommands in console := "import cats._, cats.implicits._"
