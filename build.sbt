import BuildHelper._
import Libraries._
import scala.sys.process._

ThisBuild / scalaVersion := "2.13.11"
ThisBuild / organization := "com.joli-ciel"
ThisBuild / homepage     := Some(url("https://www.joli-ciel.com/"))
ThisBuild / licenses     := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))

val cloakroomVersion = "0.5.13"
val amazonSdkVersion = "2.20.98"
val scalaXmlVersion = "2.1.0"
val jochre2Version = "2.6.2-SNAPSHOT"
val yivoTranscriberVersion = "0.1.2-SNAPSHOT"
val javaCVVersion = "1.5.9"
val scallopVersion = "5.0.0"

lazy val jochre3OCRVersion = sys.env.get("JOCHRE3_OCR_VERSION")
  .getOrElse{
    ConsoleLogger().warn("JOCHRE3_OCR_VERSION env var not found")
    "0.0.1-SNAPSHOT"
  }

val projectSettings = commonSettings ++ Seq(
  version := jochre3OCRVersion,
)

lazy val downloadZip = taskKey[Unit]("Download the zip files to resource directory")

downloadZip := {
  val lexiconPath = "modules/yiddish/resources/jochre-yiddish-lexicon-1.0.1.zip"
  if(java.nio.file.Files.notExists(new File(lexiconPath).toPath())) {
    println(f"Path ${lexiconPath} does not exist, downloading...")
    url("https://github.com/urieli/jochre/releases/download/v2.3.5/jochre-yiddish-lexicon-1.0.1.zip") #> file(lexiconPath) !
  } else {
    println(f"Path ${lexiconPath} exists, no need to download.")
  }
  val modelPath = "modules/yiddish/resources/yiddish_letter_model.zip"
  if (java.nio.file.Files.notExists(new File(modelPath).toPath())) {
    println(f"Path ${modelPath} does not exist, downloading...")
    url("https://github.com/urieli/jochre/releases/download/v2.3.5/yiddish_letter_model.zip") #> file(modelPath) !
  } else {
    println(f"Path ${modelPath} exists, no need to download.")
  }
}

(Compile / compile) := ((Compile / compile).dependsOn(downloadZip)).value

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val root =
  Project(id = "jochre3-ocr", base = file("."))
    .settings(noDoc: _*)
    .settings(noPublishSettings: _*)
    .aggregate(core, yiddish)

lazy val core = project
  .in(file("modules/core"))
  .settings(projectSettings: _*)
  .settings(
    libraryDependencies ++= commonDeps ++ httpClientDeps ++ Seq(
      "org.rogach" %% "scallop" % scallopVersion,
      "org.bytedeco" % "javacv-platform" % javaCVVersion,
      "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion,
    ),
    Compile / packageDoc / mappings := Seq(),
    fork := true,
    publish / skip  := true,
  )

lazy val yiddish = project
  .in(file("modules/yiddish"))
  .settings(projectSettings: _*)
  .settings(
    libraryDependencies ++= commonDeps ++ Seq(
      "com.joliciel.ljtrad" % "yivo-transcriber" % yivoTranscriberVersion,
      "com.joliciel.jochre" % "jochre-yiddish" % jochre2Version
    ),
    Compile / packageDoc / mappings := Seq(),
    fork := true,
    publish / skip  := true,
  )
  .dependsOn(core % "compile->compile;test->test")