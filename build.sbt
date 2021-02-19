import sbt.Keys.version

lazy val root = (project in file("."))
  .enablePlugins(SbtPlugin)
  .settings(
    name := "sbt-bulkysources",
    organization := "com.marchanka",
    version := "0.1-SNAPSHOT",
    pluginCrossBuild / sbtVersion := {
      scalaBinaryVersion.value match {
        case "2.12" => "1.2.8"
      }
    }
  )
