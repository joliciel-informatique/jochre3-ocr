import BuildHelper._
import Libraries._
import com.typesafe.sbt.packager.MappingsHelper.directory

import scala.sys.process._
import xerial.sbt.Sonatype._
import com.typesafe.sbt.packager.docker.Cmd

//ThisBuild / scalaVersion := "3.3.3"
ThisBuild / scalaVersion := "2.13.13"
ThisBuild / organization := "com.joli-ciel"
ThisBuild / homepage := Some(url("https://gitlab.com/jochre/jochre3-ocr"))
ThisBuild / licenses := List("AGPL-v3" -> url("https://www.gnu.org/licenses/agpl-3.0.en.html"))
ThisBuild / versionScheme := Some("semver-spec")

val cloakroomVersion = "0.5.13"
val amazonSdkVersion = "2.20.98"
val scalaXmlVersion = "2.1.0"
val yivoTranscriberVersion = "0.1.6"
val javaCVVersion = "1.5.9"
val scallopVersion = "5.0.0"
val apacheCommonsTextVersion = "1.11.0"
val apacheCommonsMathVersion = "3.6.1"
val apachePdfBoxVersion = "3.0.1"
val twelveMonkeysVersion = "3.10.1"
val djlVersion = "0.26.0"
val pytorchVersion = "2.1.1"
val classGraphVersion = "4.8.172"

lazy val jochre3OCRVersion = sys.env
  .get("JOCHRE3_OCR_VERSION")
  .getOrElse {
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

ThisBuild / publishTo := {
  // For accounts created after Feb 2021:
  // val nexus = "https://s01.oss.sonatype.org/"
  val nexus = "https://oss.sonatype.org/"
  if (isSnapshot.value) Some("snapshots" at nexus + "content/repositories/snapshots")
  else Some("releases" at nexus + "service/local/staging/deploy/maven2")
}
ThisBuild / credentials += Credentials(Path.userHome / ".sbt" / "sonatype_credentials_joliciel")
ThisBuild / publishTo := sonatypePublishToBundle.value
ThisBuild / sonatypeProjectHosting := Some(GitLabHosting("assafurieli", "jochre/jochre3-ocr", "assafurieli@gmail.com"))

val projectSettings = commonSettings ++ Seq(
  version := jochre3OCRVersion
)

testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")

lazy val root =
  Project(id = "jochre3-ocr", base = file("."))
    .settings(noDoc: _*)
    .settings(noPublishSettings: _*)
    .settings(
      name := "jochre3-ocr",
      publish / skip := false
    )
    .aggregate(core, yiddish, api)
    .enablePlugins(DockerForwardPlugin)

val learningDeps = Seq(
  "ai.djl" % "api" % djlVersion,
  "ai.djl" % "bom" % djlVersion,
  "ai.djl" % "basicdataset" % djlVersion,
  "ai.djl" % "model-zoo" % djlVersion,
  "ai.djl.pytorch" % "pytorch-engine" % djlVersion,
  "ai.djl.pytorch" % "pytorch-model-zoo" % djlVersion,
  "ai.djl.pytorch" % "pytorch-native-cpu" % pytorchVersion,
  "ai.djl.pytorch" % "pytorch-jni" % f"$pytorchVersion-$djlVersion"
)

val jpegDeps = Seq(
  "com.twelvemonkeys.imageio" % "imageio-jpeg" % twelveMonkeysVersion,
  "com.twelvemonkeys.imageio" % "imageio-tiff" % twelveMonkeysVersion,
  "com.github.jai-imageio" % "jai-imageio-jpeg2000" % "1.4.0",
  "org.apache.pdfbox" % "jbig2-imageio" % apachePdfBoxVersion
)

lazy val core = project
  .in(file("modules/core"))
  .settings(projectSettings: _*)
  .settings(
    name := "jochre3-ocr-core",
    libraryDependencies ++= jpegDeps ++ commonDeps ++ httpClientDeps ++ Seq(
      "org.rogach" %% "scallop" % scallopVersion,
      "org.bytedeco" % "javacv-platform" % javaCVVersion,
      "org.scala-lang.modules" %% "scala-xml" % scalaXmlVersion,
      "org.apache.commons" % "commons-text" % apacheCommonsTextVersion,
      "org.apache.commons" % "commons-math3" % apacheCommonsMathVersion,
      "org.apache.pdfbox" % "pdfbox" % apachePdfBoxVersion,
      "org.apache.pdfbox" % "pdfbox-io" % apachePdfBoxVersion,
      "io.github.classgraph" % "classgraph" % classGraphVersion
    ) ++ learningDeps,
    // Compile / packageDoc / mappings := Seq(),
    Compile / packageDoc / publishArtifact := true,
    fork := true,
    publish / skip := false
  )
  .disablePlugins(DockerPlugin)

lazy val yiddish = project
  .in(file("modules/yiddish"))
  .settings(projectSettings: _*)
  .settings(
    name := "jochre3-ocr-yiddish",
    libraryDependencies ++= commonDeps ++ Seq(
      "com.joliciel.ljtrad" % "yivo-transcriber" % yivoTranscriberVersion
    ),
    // Compile / packageDoc / mappings := Seq(),
    Compile / packageDoc / publishArtifact := true,
    fork := true,
    publish / skip := false
  )
  .dependsOn(core % "compile->compile;test->test")
  .disablePlugins(DockerPlugin)

lazy val api = project
  .in(file("modules/api"))
  .enablePlugins(JavaServerAppPackaging, DockerPlugin)
  .settings(projectSettings: _*)
  .settings(
    libraryDependencies ++= commonDeps ++ databaseDeps ++ apiDeps ++ Seq(
      "com.safety-data" %% "cloakroom-scala" % cloakroomVersion,
      "com.safety-data" %% "cloakroom-test-util-scala" % cloakroomVersion % Test,
      "com.safety-data" % "cloakroom" % cloakroomVersion,
      "com.github.cb372" %% "cats-retry" % "3.1.0"
    ),
    Docker / packageName := "jochre/jochre3-ocr",
    Docker / maintainer := "Joliciel Informatique SARL",
    Docker / daemonUserUid := Some("1001"),
    dockerBaseImage := "openjdk:17.0.2-bullseye",
    Docker / dockerRepository := sys.env.get("JOCHRE3_DOCKER_REGISTRY"),
    Docker / version := version.value,
    dockerExposedPorts := Seq(3434),
    dockerExposedVolumes := Seq("/opt/docker/index"),
    // These next line results in directory /opt/docker/models
    Universal / mappings ++= directory("modules/yiddish/resources/models"),
    // Add docker commands before changing user
    Docker / dockerCommands := dockerCommands.value.flatMap {
      case Cmd("USER", args @ _*) if args.contains("1001:0") =>
        Seq(
          // Add unattended security upgrades to docker image
          Cmd("RUN", "apt update && apt install -y unattended-upgrades"),
          Cmd("RUN", "unattended-upgrade -d --dry-run"),
          Cmd("RUN", "unattended-upgrade -d"),
          Cmd("USER", args: _*)
        )
      case cmd => Seq(cmd)
    },
    // do not package scaladoc
    Compile / packageDoc / mappings := Seq(),
    Compile / mainClass := Some("com.joliciel.jochre.ocr.api.MainApp"),
    publish / skip := true,
    fork := true
  )
  .dependsOn(core % "compile->compile;test->test", yiddish % "compile->compile;test->test")
  .enablePlugins(DockerPlugin)
