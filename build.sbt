name := """LearnCats"""

version := "1.0-SNAPSHOT"

// scalaVersion := "2.12.14"
scalaVersion := "2.13.5"

resolvers += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.typelevel" % "kind-projector" % "0.13.0" cross CrossVersion.full)

libraryDependencies += "org.typelevel" %% "cats-core" % "2.6.1"
libraryDependencies += "org.typelevel" %% "cats-free" % "2.6.1"
libraryDependencies += "org.typelevel" %% "cats-effect" % "2.5.1"
libraryDependencies += "org.typelevel" %% "cats-effect-laws" % "2.5.1"

libraryDependencies += "org.scala-lang.modules" %% "scala-swing" % "3.0.0"
libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

//scalacOptions ++= Seq("-feature", "-deprecation", "-Ypartial-unification")
scalacOptions ++= Seq("-feature", "-deprecation")
// scalacOptions += "-Ystatistics:typer"

// initialCommands in console := "import cats._, cats.implicits._"
