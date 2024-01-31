package com.joliciel.jochre.ocr.api.analysis

import com.joliciel.jochre.ocr.api.HttpError.BadRequest
import com.joliciel.jochre.ocr.api.Types.Requirements
import com.joliciel.jochre.ocr.api.{HttpError, HttpErrorProtocol}
import sttp.capabilities.zio.ZioStreams
import sttp.model.StatusCode
import sttp.tapir.generic.auto._
import sttp.tapir.json.zio.jsonBody
import sttp.tapir.ztapir.{ZServerEndpoint, oneOfVariant, streamTextBody, endpoint => tapirEndpoint, _}
import sttp.tapir.{AnyEndpoint, CodecFormat, PublicEndpoint, multipartBody, oneOf}
import zio.stream.ZStream

import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext

case class AnalysisApp( executionContext: ExecutionContext)
  extends HttpErrorProtocol
    with AnalysisLogic
    with AnalysisApiProtocol
{
  implicit val ec: ExecutionContext = executionContext

  val postAnalyzeFileEndpoint: PublicEndpoint[FileForm, HttpError, ZStream[Any, Throwable, Byte], Any with ZioStreams]  =
    tapirEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .post
      .in("ocr")
      .in("file")
      .in(multipartBody[FileForm])
      .out(streamTextBody(ZioStreams)(CodecFormat.Xml(), Some(StandardCharsets.UTF_8)))
      .description("Post an image file for analysis and return xml result.")

  val postAnalyzeFileHttp: ZServerEndpoint[Requirements, Any with ZioStreams] =
    postAnalyzeFileEndpoint.zServerLogic(input => postAnalyzeFileLogic(input))

  val postAnalyzeURLEndpoint: PublicEndpoint[AnalyseURLRequest, HttpError, ZStream[Any, Throwable, Byte], Any with ZioStreams] =
    tapirEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .post
      .in("ocr")
      .in("url")
      .in(jsonBody[AnalyseURLRequest].example(AnalysisHelper.analyzeURLRequestExample))
      .out(streamTextBody(ZioStreams)(CodecFormat.Xml(), Some(StandardCharsets.UTF_8)))
      .description("Post an image URL for analysis and return xml result.")

  val postAnalyzeURLHttp: ZServerEndpoint[Requirements, Any with ZioStreams] =
    postAnalyzeURLEndpoint.zServerLogic(input => postAnalyzeURLLogic(input))

  val endpoints: List[AnyEndpoint] = List(
    postAnalyzeFileEndpoint,
    postAnalyzeURLEndpoint
  )

  val http: List[ZServerEndpoint[Requirements, Any with ZioStreams]] = List(
    postAnalyzeFileHttp,
    postAnalyzeURLHttp
  )
}