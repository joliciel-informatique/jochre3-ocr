package com.joliciel.jochre.ocr.api.analysis

import com.joliciel.jochre.ocr.core.output.OutputFormat
import sttp.model.MediaType
import sttp.tapir.{Codec, CodecFormat, DecodeResult, Schema}
import sttp.tapir.SchemaType.SString

trait AnalysisSchemaSupport {
  val ZipCodecFormat: CodecFormat = new CodecFormat {
    override def mediaType: MediaType = MediaType.ApplicationZip
  }

  given Codec[String, OutputFormat, CodecFormat.TextPlain] =
    Codec.string.mapDecode(s => DecodeResult.Value(OutputFormat.withName(s)))(_.entryName)
  given Schema[OutputFormat] = Schema(SString(), description = Some("OutputFormat"))
}
