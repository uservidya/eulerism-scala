import de.johoop.jacoco4sbt._
import JacocoPlugin._

name := "eulerism-scala"

version := "0.1"

scalaVersion := "2.10.3"

//crossScalaVersions := Seq("2.10.3")

scalacOptions ++= Seq("-deprecation", "-unchecked", "-feature", "-optimise", "-Yinline-warnings")

libraryDependencies ++= Seq(
  "org.spire-math" %% "spire" % "0.6.0",
  "org.scalaz" %% "scalaz-core" % "7.0.5",
  "org.specs2" %% "specs2-scalacheck" % "2.3.4" % "test",
  "org.specs2" %% "specs2-matcher" % "2.3.4" % "test",
  "org.specs2" %% "specs2-junit" % "2.3.4" % "test",
  "org.scalacheck" %% "scalacheck" % "1.11.1" % "test"
)

jacoco.settings

jacoco.reportFormats in jacoco.Config := Seq(
  XMLReport(encoding = "utf-8"),
  ScalaHTMLReport(withBranchCoverage = true))
