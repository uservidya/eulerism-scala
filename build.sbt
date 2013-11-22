name := "eulerism-scala"

version := "0.1"

scalaVersion := "2.10.3"

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.6.0",
  "org.scalaz" %% "scalaz-core" % "7.0.4",
  "org.specs2" %% "specs2" % "2.3.4" % "test"
)
