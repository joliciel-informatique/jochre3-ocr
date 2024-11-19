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

  implicit val jsonDecoder_wordFrequency: JsonDecoder[WordFrequency] =
    DeriveJsonDecoder.gen[WordFrequency]
  implicit val jsonEncoder_wordFrequency: JsonEncoder[WordFrequency] =
    DeriveJsonEncoder.gen[WordFrequency]

  implicit val jsonDecoder_wordsInLexiconResponse: JsonDecoder[WordsInLexiconResponse] =
    DeriveJsonDecoder.gen[WordsInLexiconResponse]
  implicit val jsonEncoder_wordsInLexiconResponse: JsonEncoder[WordsInLexiconResponse] =
    DeriveJsonEncoder.gen[WordsInLexiconResponse]

}
