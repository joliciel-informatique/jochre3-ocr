package com.joliciel.jochre.ocr.api.analysis

import zio.json._

trait AnalysisApiProtocol {
  implicit val jsonDecoder_analyseURLRequest: JsonDecoder[AnalyseURLRequest] = DeriveJsonDecoder.gen[AnalyseURLRequest]
  implicit val jsonEncoder_analyseURLRequest: JsonEncoder[AnalyseURLRequest] = DeriveJsonEncoder.gen[AnalyseURLRequest]
}
