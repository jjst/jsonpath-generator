onChangedBuildSource := ReloadOnSourceChanges

organization    := "eu.jjst"
scalaVersion    := "2.13.4"
name := "jsonpath-generator"

libraryDependencies ++= Seq(
  "org.json" % "json" % "20210307"
)
