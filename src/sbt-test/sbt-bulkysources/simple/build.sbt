import sbt.TaskKey

scalaVersion := "2.12.9"

val objectWith10Lines = (10, "ObjectWith10Lines")
val objectWith50Lines = (50, "ObjectWith50Lines")
val objectWith150Lines = (150, "ObjectWith150Lines")
val specWith10Lines = (10, "ObjectWith10LinesSpec")
val specWith50Lines = (50, "ObjectWith50LinesSpec")
val specWith150Lines = (150, "ObjectWith150LinesSpec")

lazy val withoutThresholdOverride = project.settings(
  TaskKey[Unit]("testDefaultThreshold") := {
    require(
      bulkySourcesThresholdInLines.value == 100, s"The default threshold not 100"
    )
  }
)

lazy val withThresholdOverride = project
  .settings(
    bulkySourcesThresholdInLines := 20,
    TaskKey[Unit]("testWithEmptyThresholdInUserInput") := {
      val result = (Compile / bulkySources).toTask("").value
      verifyResult(result, List(objectWith150Lines, objectWith50Lines))
    },
    TaskKey[Unit]("testWithSpecifiedInvalidThresholdInUserInput") := {
      val result = (Compile / bulkySources).toTask(" laskdjfklsajdfllkj").value
      verifyResult(result, List(objectWith150Lines, objectWith50Lines))
    },
    TaskKey[Unit]("testWithCompileConfiguration") := {
      val result = (Compile / bulkySources).toTask(" 10").value
      verifyResult(result, List(objectWith150Lines, objectWith50Lines, objectWith10Lines))
    },
    TaskKey[Unit]("testWithTestConfiguration") := {
      val result = (Test / bulkySources).toTask(" 10").value
      verifyResult(result, List(specWith150Lines, specWith50Lines, specWith10Lines))
    }
  )

def verifyResult(actualResult: Seq[(Int, File)], expectedResult: List[(Int, String)]) = {
  require(
    actualResult.size == expectedResult.size,
    s"The plugin output return more/less elements than expected: $actualResult vs $expectedResult"
  )
  actualResult.indices.foreach { i =>
    val (expectedSize, expectedFileName) = expectedResult(i)
    val (actualSize, actualFile) = actualResult(i)
    require(
      actualSize == expectedSize,
      s"The file sizes are different: ${actualResult(i)} vs ${expectedResult(i)}"
    )
    require(
      actualFile.getAbsolutePath.contains(expectedFileName),
      s"The file names are different: ${actualFile.getAbsolutePath} vs $expectedFileName"
    )
  }
}
