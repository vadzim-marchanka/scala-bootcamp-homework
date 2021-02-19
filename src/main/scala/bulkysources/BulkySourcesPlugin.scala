package bulkysources

import sbt.Def.spaceDelimited

import scala.io.Source
import scala.util.Try
import sbt.Keys.{sources, streams}
import sbt._

object BulkySourcesPlugin extends AutoPlugin {
  override def trigger = allRequirements
  override val requires: Plugins = plugins.JvmPlugin

  object autoImport {
    val bulkySourcesThresholdInLines = settingKey[Int]("Configures the user threshold for bulkySources task")
    val bulkySources = inputKey[Seq[(Int, File)]](
      "Shows the list of bulky sources with line number more than threshold number configured in bulkySourcesThresholdInLines setting."
    )
  }

  import autoImport._
  override lazy val globalSettings: Seq[Setting[_]] = Seq(
    bulkySourcesThresholdInLines := 100
  )

  override lazy val projectSettings: Seq[Setting[_]] =
    inConfig(Compile)(bulkySourcesTasks) ++ inConfig(Test)(bulkySourcesTasks)

  private val bulkySourcesTasks = Seq(
    bulkySources := {
      val userThreshold: Option[String] = spaceDelimited("<arg>").parsed.headOption
      val defaultThreshold = bulkySourcesThresholdInLines.value
      val sourceFiles = sources.value
      val s = streams.value

      def determineThreshold: Int = {
        userThreshold match {
          case None =>
            s.log.info(s"There is no user specified threshold. The default one is used: $defaultThreshold.")
            defaultThreshold
          case Some(strThreshold) =>
            Try(strThreshold.toInt).toOption match {
              case Some(value) => value
              case None =>
                s.log.error(s"Can not parse the user threshold: $strThreshold. Default threshold is used: $defaultThreshold.")
                defaultThreshold
            }
        }
      }

      def filterAndSortSourceFilesBySize(threshold: Int): Seq[(Int, File)] = {
        val filesWithSizeAttempt: Seq[(File, Try[Int])] = sourceFiles.map { file =>
          // todo: add right logic for try with resource. Using is accessible starting from scala 2.13.0
          val sizeAttempt = Try(Source.fromFile(file, "UTF-8")).map(_.getLines.size)
          (file, sizeAttempt)
        }

        val (filesWithSize, filesWithErrors) = filesWithSizeAttempt.partition { case (_, sizeAttempt) => sizeAttempt.isSuccess }

        filesWithErrors.foreach { case (file, exception) =>
          s.log.warn(s"Can not handle file  $file , because of the exception ${exception.failed.get}")
        }

        val filteredSizeWithFiles = filesWithSize
          .map { case (file, maybeSize) => (maybeSize.get, file) }
          .filter { case (size, _) => size >= threshold }
          .sortBy { case (size, _) => -size }

        filteredSizeWithFiles
      }

      filterAndSortSourceFilesBySize(determineThreshold)
    }
  )
}
