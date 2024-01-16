import BuildHelper._
import Libraries._
import scala.sys.process._
import xerial.sbt.Sonatype._

ThisBuild / scalaVersion := "2.13.11"
ThisBuild / organization := "com.joli-ciel"
ThisBuild / homepage     := Some(url("https://www.joli-ciel.com/"))
ThisBuild / licenses     := List("Apache-2.0" -> url("https://www.apache.org/licenses/LICENSE-2.0"))
ThisBuild / versionScheme := Some("semver-spec")

val cloakroomVersion = "0.5.13"
val amazonSdkVersion = "2.20.98"
val scalaXmlVersion = "2.1.0"
val jochre2Version = "2.6.4"
val yivoTranscriberVersion = "0.1.2"
val javaCVVersion = "1.5.9"
val scallopVersion = "5.0.0"
val apacheCommonsTextVersion = "1.11.0"

lazy val jochre3OCRVersion = sys.env.get("JOCHRE3_OCR_VERSION")
  .getOrElse{
    ConsoleLogger().warn("JOCHRE3_OCR_VERSION env var not found")
    "0.0.1-SNAPSHOT"
  }

ThisBuild / version := jochre3OCRVersion

ThisBuild / organization := "com.joliciel"
ThisBuild / organizationName := "Joliciel Informatique SARL"
ThisBuild / organizationHomepage := Some(url("https://joli-ciel.com/"))

ThisBuild / scmInfo := Some(
  ScmInfo(
    url("https://gitlab.com/jochre/jochre3-ocr"),
    "scm:git@gitlab.com:jochre/jochre3-ocr.git"
  )
)
ThisBuild / developers := List(
  Developer(
    id = "assafurieli@gmail.com",
    name = "Assaf Urieli",
    email = "assafurieli@gmail.com",
    url = url("https://gitlab.com/assafurieli")
  )
)

ThisBuild / description := "Jochre3 OCR engine with default implementation for Yiddish"
ThisBuild / licenses := List(
  "GNU Affero General Public License Version 3" -> new URL("http://www.gnu.org/licenses/agpl-3.0.html")
)
ThisBuild / homepage := Some(url("https://gitlab.com/jochre/jochre3-ocr"))

// Remove all additional repository other than Maven Central from POM
ThisBuild / pomIncludeRepository := { _ => false }
ThisBuild / publishTo := {
  // For accounts created after Feb 2021:
  //val nexus = "https://s01.oss.sonatype.org/"
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / publishMavenStyle := true
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials_joliciel")
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / sonatypeProjectHosting := Some(GitLabHosting("assafurieli", "jochre/jochre3-ocr", "assafurieli@gmail.com"))

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
    .settings(
      name := "jochre3-ocr",
      publish / skip := false,
    )
    .aggregate(core, yiddish)

lazy val core = project
  .in(file("modules/core"))
  .settings(projectSettings: _*)
  .settings(
    name := "jochre3-ocr-core",
    libraryDependencies ++= commonDeps ++ httpClientDeps ++ Seq(
      "org.rogach" %% "scallop" % scallopVersion,
      "org.bytedeco" % "javacv-platform" % javaCVVersion,
      "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion,
      "org.apache.commons" % "commons-text" % apacheCommonsTextVersion,
    ),
    //Compile / packageDoc / mappings := Seq(),
    Compile / packageDoc / publishArtifact := true,
    fork := true,
    publish / skip  := false,
  )

lazy val yiddish = project
  .in(file("modules/yiddish"))
  .settings(projectSettings: _*)
  .settings(
    name := "jochre3-ocr-yiddish",
    libraryDependencies ++= commonDeps ++ Seq(
      "com.joliciel.ljtrad" % "yivo-transcriber" % yivoTranscriberVersion,
      "com.joliciel.jochre" % "jochre-yiddish" % jochre2Version
    ),
    //Compile / packageDoc / mappings := Seq(),
    Compile / packageDoc / publishArtifact := true,
    fork := true,
    publish / skip  := false,
  )
  .dependsOn(core % "compile->compile;test->test")