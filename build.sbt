scalaVersion := "2.13.3"


name := "scala-bootcamp-homework"
organization := "com.marchanka"
version := "0.1"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "3.2.2" % Test,
  "org.scalacheck" %% "scalacheck" % "1.14.1" % Test
)
