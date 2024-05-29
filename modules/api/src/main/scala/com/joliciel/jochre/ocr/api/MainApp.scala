package com.joliciel.jochre.ocr.api

import cats.syntax.all._
import com.comcast.ip4s.{Host, Port}
import com.joliciel.jochre.ocr.api.Types.AppTask
import com.joliciel.jochre.ocr.api.analysis.AnalysisApp
import com.joliciel.jochre.ocr.yiddish.JochreYiddishFull
import com.typesafe.config.ConfigFactory
import org.http4s.HttpRoutes
import org.http4s.ember.server.EmberServerBuilder
import org.http4s.headers.Origin
import org.http4s.implicits._
import org.http4s.server.Router
import org.http4s.server.middleware.CORS
import org.slf4j.LoggerFactory
import sttp.tapir.server.http4s.ztapir.ZHttp4sServerInterpreter
import sttp.tapir.swagger.bundle.SwaggerInterpreter
import zio._
import zio.config.typesafe.TypesafeConfigProvider
import zio.interop.catz._

import scala.concurrent.duration._
import scala.jdk.CollectionConverters._

object MainApp extends ZIOAppDefault {
  private val log = LoggerFactory.getLogger(getClass)
  private val config = ConfigFactory.load().getConfig("jochre.ocr.api")

  override val bootstrap: ZLayer[ZIOAppArgs, Throwable, Any] =
    Runtime.setConfigProvider(TypesafeConfigProvider.fromTypesafeConfig(config))

  private def runServer(executor: Executor): Task[Unit] = {
    val analysisDirectives: AnalysisApp = AnalysisApp(
      executor.asExecutionContext
    )
    val analysisRoutes: HttpRoutes[AppTask] =
      ZHttp4sServerInterpreter().from(analysisDirectives.http).toRoutes

    val version = sys.env.getOrElse("JOCHRE3_OCR_VERSION", "0.1.0-SNAPSHOT")
    val swaggerDirectives = SwaggerInterpreter().fromEndpoints[AppTask](
      analysisDirectives.endpoints,
      "Jochre OCR Server",
      version
    )
    val swaggerRoutes: HttpRoutes[AppTask] =
      ZHttp4sServerInterpreter().from(swaggerDirectives).toRoutes

    val routes = analysisRoutes <+> swaggerRoutes

    val httpApp = Router("/" -> routes).orNotFound

    val corsPolicy = CORS.policy
      .withAllowCredentials(false)
      .withMaxAge(DurationInt(1).day)

    val hosts = config.getStringList("allow-origin-hosts").asScala.toSet

    val corsPolicyWithHosts = if (hosts.isEmpty) {
      log.info("Allowing all origins")
      corsPolicy.withAllowOriginAll
    } else {
      log.info(f"Parsing origins: ${hosts.mkString(", ")}")
      corsPolicy.withAllowOriginHost(
        hosts.flatMap { host =>
          Origin.parse(host) match {
            case Left(parseFailure) =>
              throw new Exception(
                f"Cannot parse $host as host: ${parseFailure.details}"
              )
            case Right(Origin.HostList(hosts)) =>
              log.info(f"Allowing origins: $hosts")
              hosts.toList
            case Right(Origin.Null) =>
              log.info(f"Null origin")
              Seq()
          }
        }
      )
    }

    val corsService = corsPolicyWithHosts.apply(httpApp)

    val jochreLayer = JochreYiddishFull.jochreYiddishLayer

    // Starting the server
    val server = EmberServerBuilder
      .default[AppTask]
      .withHost(Host.fromString(config.getString("host")).get)
      .withPort(Port.fromInt(config.getInt("port")).get)
      .withHttpApp(corsService)
      .build
      .allocated
      .map(_._1) *> ZIO.never

    server
      .provide(
        jochreLayer
      )
  }

  private def app: Task[Unit] = {
    Runtime.setReportFatal { t =>
      t.printStackTrace()
      try {
        java.lang.System.exit(-1)
        throw t
      } catch { case _: Throwable => throw t }
    }
    for {
      executor <- ZIO.executor
      server <- runServer(executor)
    } yield server
  }.tapErrorCause(error => ZIO.logErrorCause(s"Unable to build server", error))

  override def run: URIO[Any, ExitCode] =
    app.exitCode
}
