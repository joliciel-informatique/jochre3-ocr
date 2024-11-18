package com.joliciel.jochre.ocr.api.analysis

import zio.json._

trait AnalysisApiProtocol {
  implicit val jsonDecoder_analyseURLRequest: JsonDecoder[AnalyseURLRequest] =
    DeriveJsonDecoder.gen[AnalyseURLRequest]
  implicit val jsonEncoder_analyseURLRequest: JsonEncoder[AnalyseURLRequest] =
    DeriveJsonEncoder.gen[AnalyseURLRequest]

  implicit val jsonDecoder_analyseURLRequestWithOutputFormats: JsonDecoder[AnalyseURLRequestWithOutputFormats] =
    DeriveJsonDecoder.gen[AnalyseURLRequestWithOutputFormats]
  implicit val jsonEncoder_analyseURLRequestWithOutputFormats: JsonEncoder[AnalyseURLRequestWithOutputFormats] =
    DeriveJsonEncoder.gen[AnalyseURLRequestWithOutputFormats]

  implicit val jsonDecoder_wordInLexiconResponse: JsonDecoder[WordInLexiconResponse] =
    DeriveJsonDecoder.gen[WordInLexiconResponse]
  implicit val jsonEncoder_wordInLexiconResponse: JsonEncoder[WordInLexiconResponse] =
    DeriveJsonEncoder.gen[WordInLexiconResponse]

}
