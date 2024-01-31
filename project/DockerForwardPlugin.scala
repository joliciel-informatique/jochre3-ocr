import com.typesafe.sbt.packager.docker.DockerPlugin
import com.typesafe.sbt.packager.docker.DockerPlugin.autoImport.Docker
import sbt.Keys.publish
import sbt.{AutoPlugin, taskKey}

// From https://github.com/sbt/sbt-native-packager/issues/974#issuecomment-754129085
object DockerForwardPlugin extends AutoPlugin {
  override val requires = DockerPlugin
  override val trigger = allRequirements

  object autoImport {
    val forwardedDockerPublish = taskKey[Unit]("Docker/publish for projects with DockerPlugin")
  }
  import autoImport._

  override val projectSettings = Seq(
    forwardedDockerPublish := (Docker / publish).value
  )
}