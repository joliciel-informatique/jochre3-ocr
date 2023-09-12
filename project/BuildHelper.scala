import sbt.Keys._
import sbt._

object BuildHelper {

  val commonSettings = Seq(
    javacOptions ++= Seq("-source", "17", "-target", "17"),
    resolvers += "Typesafe Repository" at "https://repo.typesafe.com/typesafe/releases/",
    resolvers += "Sonatype Snapshots" at "https://oss.sonatype.org/content/repositories/snapshots/",
    resolvers += "Sonatype Releases" at "https://s01.oss.sonatype.org/content/repositories/releases/",
    (Test / parallelExecution) := true,
    (Test / fork)              := true
  ) ++ noDoc

  lazy val noDoc = Seq(
    (Compile / doc / sources)                := Seq.empty,
    (Compile / packageDoc / publishArtifact) := false,
  )

  /**
   * Copied from Cats
   */
  lazy val noPublishSettings = Seq(
    publish         := {},
    publishLocal    := {},
    publishM2       := {},
    publishArtifact := false,
    publish / skip  := true
  )

  testFrameworks += new TestFramework("zio.test.sbt.ZTestFramework")
}
