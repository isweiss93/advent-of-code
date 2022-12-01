ThisBuild / scalaVersion := "3.2.0"

lazy val root = (project in file("."))
  .aggregate(aoc2022)

lazy val aoc2022 = (project in file("2022"))