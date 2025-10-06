ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.3"

ThisBuild / scalacOptions += "-language:postfixOps"

lazy val root = project
  .in(file("."))
  .settings(
    name := "aoc"
  )

libraryDependencies ++= Seq(
  "org.scalacheck" %% "scalacheck" % "1.19.0",
  "org.scala-graph" %% "graph-core" % "2.0.3",
  "org.scala-graph" %% "graph-dot" % "2.0.0",
  "org.scalameta" %% "munit" % "1.2.0" % Test,
  "org.scalacheck" %% "scalacheck" % "1.19.0" % Test,
  "org.scalameta" %% "munit-scalacheck" % "1.2.0" % Test,
)
