name := """LearnCats"""

version := "1.0-SNAPSHOT"

scalaVersion := "2.12.14"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.8")

libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.6.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.1"
libraryDependencies += "org.typelevel" %% "cats-effect-laws" % "2.5.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

scalacOptions ++= Seq("-feature", "-deprecation", "-Ypartial-unification")
// scalacOptions += "-Ystatistics:typer"

// initialCommands in console := "import cats._, cats.implicits._"

