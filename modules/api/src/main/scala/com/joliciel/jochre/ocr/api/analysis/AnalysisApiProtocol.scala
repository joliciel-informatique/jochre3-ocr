package com.joliciel.jochre.ocr.api.analysis

import zio.json._

trait AnalysisApiProtocol {
  given JsonDecoder[AnalyseURLRequest] = DeriveJsonDecoder.gen[AnalyseURLRequest]
  given JsonEncoder[AnalyseURLRequest] = DeriveJsonEncoder.gen[AnalyseURLRequest]

  given JsonDecoder[AnalyseURLRequestWithOutputFormats] = DeriveJsonDecoder.gen[AnalyseURLRequestWithOutputFormats]
  given JsonEncoder[AnalyseURLRequestWithOutputFormats] = DeriveJsonEncoder.gen[AnalyseURLRequestWithOutputFormats]

  given JsonDecoder[WordInLexiconResponse] = DeriveJsonDecoder.gen[WordInLexiconResponse]
  given JsonEncoder[WordInLexiconResponse] = DeriveJsonEncoder.gen[WordInLexiconResponse]

  given JsonDecoder[WordFrequency] = DeriveJsonDecoder.gen[WordFrequency]
  given JsonEncoder[WordFrequency] = DeriveJsonEncoder.gen[WordFrequency]

  given JsonDecoder[WordsInLexiconResponse] = DeriveJsonDecoder.gen[WordsInLexiconResponse]
  given JsonEncoder[WordsInLexiconResponse] = DeriveJsonEncoder.gen[WordsInLexiconResponse]

  given JsonDecoder[StandardizedWordsResponse] = DeriveJsonDecoder.gen[StandardizedWordsResponse]
  given JsonEncoder[StandardizedWordsResponse] = DeriveJsonEncoder.gen[StandardizedWordsResponse]
}
