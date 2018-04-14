name := "Ora4s"

organization := "com.kantefier"

version := "0.0.1-SNAPSHOT"

lazy val root = (project in file("."))
  .settings(
    scalaVersion := "2.12.2",
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    libraryDependencies ++= Seq(
      "org.scala-lang" % "scala-reflect" % "2.12.2",
      "com.beachape" %% "enumeratum" % "1.5.13",
      "joda-time" % "joda-time" % "2.9.9",
      "org.joda" % "joda-convert" % "1.8.1",

      "org.scalatest" %% "scalatest" % "3.0.4" % "test",
      "org.scalamock" %% "scalamock" % "4.0.0" % "test"
      )
    )
  