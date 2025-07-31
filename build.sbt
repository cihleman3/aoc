ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.1"

ThisBuild / scalacOptions += "-language:postfixOps"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc"
  )

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.18.1",
  "org.scala-graph" %% "graph-core" % "2.0.3",
  "org.scala-graph" %% "graph-dot" % "2.0.0",
  "org.scalameta" %% "munit" % "1.1.1" % Test,
  "org.scalacheck" %% "scalacheck" % "1.18.1" % Test,
  "org.scalameta" %% "munit-scalacheck" % "1.1.0" % Test,
)
