scalaVersion := "2.13.3"


name := "scala-bootcamp-homework"
organization := "com.marchanka"
version := "0.1"

val scalaTestPlusVersion = "3.1.0.0-RC2"
val scalaTestVersion = "3.2.2"

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % scalaTestVersion % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % scalaTestPlusVersion % Test,
)
