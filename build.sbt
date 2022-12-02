ThisBuild / scalaVersion := "2.13.10"

lazy val root = (project in file("."))
  .aggregate(aoc2022)

lazy val aoc2022 = (project in file("2022")).settings(
  libraryDependencies ++= Seq(
    "com.beachape" %% "enumeratum" % "1.7.0"
  )
)