onChangedBuildSource := ReloadOnSourceChanges

organization    := "eu.jjst"
scalaVersion    := "2.13.4"
name := "jsonpath-generator"

libraryDependencies ++= Seq(
  "org.json" % "json" % "20210307"
)

val circeVersion = "0.14.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser"
).map(_ % circeVersion)

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.3.0"
