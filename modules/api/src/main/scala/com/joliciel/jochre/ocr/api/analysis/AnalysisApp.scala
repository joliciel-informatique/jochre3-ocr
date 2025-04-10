package com.joliciel.jochre.ocr.api.analysis

import com.joliciel.jochre.ocr.api.HttpError.BadRequest
import com.joliciel.jochre.ocr.api.Types.Requirements
import com.joliciel.jochre.ocr.api.{HttpError, HttpErrorProtocol}
import com.joliciel.jochre.ocr.core.output.OutputFormat
import sttp.capabilities.zio.ZioStreams
import sttp.model.{Header, HeaderNames, MediaType, StatusCode}
import sttp.tapir.generic.auto._
import sttp.tapir.json.zio.jsonBody
import sttp.tapir.ztapir.{ZServerEndpoint, oneOfVariant, streamTextBody, endpoint => tapirEndpoint, _}
import sttp.tapir.{AnyEndpoint, CodecFormat, PublicEndpoint, multipartBody, oneOf}
import zio.stream.ZStream

import java.nio.charset.StandardCharsets
import scala.concurrent.ExecutionContext

case class AnalysisApp(executionContext: ExecutionContext)
    extends HttpErrorProtocol
    with AnalysisLogic
    with AnalysisSchemaSupport
    with AnalysisApiProtocol {
  given ExecutionContext = executionContext

  val postAnalyzeFileEndpoint: PublicEndpoint[FileForm, HttpError, ZStream[
    Any,
    Throwable,
    Byte
  ], Any & ZioStreams] =
    tapirEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .post
      .in("ocr")
      .in("file")
      .in(
        multipartBody[FileForm].description(
          """start (optional): for PDF files only, the start page.<br>
          | end (optional): for PDF files only, the end page.<br>
          | dpi (optional): for PDF files only, the DPI (defaults to 300)""".stripMargin
        )
      )
      .out(
        streamTextBody(ZioStreams)(
          CodecFormat.Xml(),
          Some(StandardCharsets.UTF_8)
        )
      )
      .description("Post an image file for analysis and return xml result.")

  val postAnalyzeFileHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    postAnalyzeFileEndpoint.zServerLogic(input => postAnalyzeFileLogic(input))

  val postAnalyzeFileWithOutputFormatsEndpoint: PublicEndpoint[
    FileFormWithOutputFormats,
    HttpError,
    (
        ZStream[
          Any,
          Throwable,
          Byte
        ],
        String
    ),
    Any & ZioStreams
  ] =
    tapirEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .post
      .in("ocr")
      .in("file")
      .in("zip")
      .in(
        multipartBody[FileFormWithOutputFormats].description(
          f"""outputFormats: Comma-delimited list containing any of: ${OutputFormat.values
            .map(_.entryName)
            .mkString(", ")}.<br>
            |start (optional): for PDF files only, the start page.<br>
            | end (optional): for PDF files only, the end page.<br>
            | dpi (optional): for PDF files only, the DPI (defaults to 300)""".stripMargin
        )
      )
      .out(streamBinaryBody(ZioStreams)(ZipCodecFormat))
      .out(header(Header.contentType(MediaType.ApplicationZip)))
      .out(header[String](HeaderNames.ContentDisposition))
      .description("Post an image file for analysis and return requested outputs in zip file.")

  val postAnalyzeFileWithOutputFormatsHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    postAnalyzeFileWithOutputFormatsEndpoint.zServerLogic(input => postAnalyzeFileWithOutputFormatsLogic(input))

  val postAnalyzeURLEndpoint: PublicEndpoint[
    AnalyseURLRequest,
    HttpError,
    ZStream[Any, Throwable, Byte],
    Any & ZioStreams
  ] =
    tapirEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .post
      .in("ocr")
      .in("url")
      .in(
        jsonBody[AnalyseURLRequest].example(
          AnalysisHelper.analyzeURLRequestExample
        )
      )
      .out(
        streamTextBody(ZioStreams)(
          CodecFormat.Xml(),
          Some(StandardCharsets.UTF_8)
        )
      )
      .description("Post an image URL for analysis and return xml result.")

  val postAnalyzeURLHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    postAnalyzeURLEndpoint.zServerLogic(input => postAnalyzeURLLogic(input))

  val postAnalyzeURLWithOutputFormatsEndpoint: PublicEndpoint[
    AnalyseURLRequestWithOutputFormats,
    HttpError,
    (ZStream[Any, Throwable, Byte], String),
    Any & ZioStreams
  ] =
    tapirEndpoint
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .post
      .in("ocr")
      .in("url")
      .in("zip")
      .in(
        jsonBody[AnalyseURLRequestWithOutputFormats].example(
          AnalysisHelper.analyzeURLRequestWithOutputFormatsExample
        )
      )
      .out(streamBinaryBody(ZioStreams)(ZipCodecFormat))
      .out(header(Header.contentType(MediaType.ApplicationZip)))
      .out(header[String](HeaderNames.ContentDisposition))
      .description("Post an image URL for analysis and return requested outputs in zip file.")

  val postAnalyzeURLWithOutputFormatsHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    postAnalyzeURLWithOutputFormatsEndpoint.zServerLogic(input => postAnalyzeURLWithOutputFormatsLogic(input))

  val getWordInLexiconEndpoint: PublicEndpoint[String, HttpError, WordInLexiconResponse, Any] =
    tapirEndpoint.get
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .in("word-in-lexicon")
      .in(query[String]("word").description("The word to check").example("שױן"))
      .out(jsonBody[WordInLexiconResponse].example(WordInLexiconResponse(1)))
      .description(
        "Check if the word is in the lexicon." +
          " If the frequency is greater than 0, the word is in the lexicon." +
          " If the frequency is equal to 0, the word is not in the lexicon." +
          " If the frequency is less than 0, the word is deemed impossible."
      )

  val getWordInLexiconHttp: ZServerEndpoint[Requirements, Any] =
    getWordInLexiconEndpoint.zServerLogic(input => getWordInLexiconLogic(input))

  val getWordsInLexiconEndpoint: PublicEndpoint[List[String], HttpError, WordsInLexiconResponse, Any] =
    tapirEndpoint.get
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .in("words-in-lexicon")
      .in(query[List[String]]("words").description("The words to check").example(List("שױן", "זשרעזש", "פ3ןא")))
      .out(
        jsonBody[WordsInLexiconResponse].example(
          WordsInLexiconResponse(Seq(WordFrequency("שױן", 1), WordFrequency("זשרעזש", 0), WordFrequency("פ3ןא", -1)))
        )
      )
      .description(
        "Check if the words are in the lexicon." +
          " If the frequency is greater than 0, the word is in the lexicon." +
          " If the frequency is equal to 0, the word is not in the lexicon." +
          " If the frequency is less than 0, the word is deemed impossible."
      )

  val getWordsInLexiconHttp: ZServerEndpoint[Requirements, Any] =
    getWordsInLexiconEndpoint.zServerLogic(input => getWordsInLexiconLogic(input))

  val getStandardizeWordsEndpoint: PublicEndpoint[List[String], HttpError, StandardizedWordsResponse, Any] =
    tapirEndpoint.get
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .in("standardize")
      .in(query[List[String]]("words").description("The words to standardize").example(List("איהר", "איר", "האבּן")))
      .out(
        jsonBody[StandardizedWordsResponse].example(
          StandardizedWordsResponse(Seq("איר", "איר", "האָבן"))
        )
      )
      .description(
        "Standardize a sequence of words"
      )

  val getStandardizeWordsHttp: ZServerEndpoint[Requirements, Any] =
    getStandardizeWordsEndpoint.zServerLogic(input => getStandardizeWordsLogic(input))

  val postDehyphenateEndpoint: PublicEndpoint[TextFileForm, HttpError, ZStream[
    Any,
    Throwable,
    Byte
  ], Any & ZioStreams] =
    tapirEndpoint.post
      .errorOut(
        oneOf[HttpError](
          oneOfVariant[BadRequest](StatusCode.BadRequest, jsonBody[BadRequest])
        )
      )
      .in("dehyphenate")
      .in(multipartBody[TextFileForm].description("The text file to dehyphenate"))
      .out(
        streamTextBody(ZioStreams)(
          CodecFormat.TextPlain(),
          Some(StandardCharsets.UTF_8)
        )
      )
      .description(
        "Dehyphenate a file, attempting to recognize hard and soft hyphens."
      )

  val postDehyphenateHttp: ZServerEndpoint[Requirements, Any & ZioStreams] =
    postDehyphenateEndpoint.zServerLogic(input => postDehyphenateLogic(input))

  val endpoints: List[AnyEndpoint] = List(
    postAnalyzeFileEndpoint,
    postAnalyzeURLEndpoint,
    postAnalyzeFileWithOutputFormatsEndpoint,
    postAnalyzeURLWithOutputFormatsEndpoint,
    getWordInLexiconEndpoint,
    getWordsInLexiconEndpoint,
    getStandardizeWordsEndpoint,
    postDehyphenateEndpoint
  )

  val http: List[ZServerEndpoint[Requirements, Any & ZioStreams]] = List(
    postAnalyzeFileHttp,
    postAnalyzeURLHttp,
    postAnalyzeFileWithOutputFormatsHttp,
    postAnalyzeURLWithOutputFormatsHttp,
    getWordInLexiconHttp,
    getWordsInLexiconHttp,
    getStandardizeWordsHttp,
    postDehyphenateHttp
  )
}
