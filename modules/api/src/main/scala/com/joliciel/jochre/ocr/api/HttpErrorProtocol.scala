package com.joliciel.jochre.ocr.api

import com.joliciel.jochre.ocr.api.HttpError.{BadRequest, Conflict, NotFound, Unauthorized}
import zio.json._

trait HttpErrorProtocol {
  implicit val jsonDecoder_badRequest: JsonDecoder[BadRequest] =
    DeriveJsonDecoder.gen[BadRequest]
  implicit val jsonEncoder_badRequest: JsonEncoder[BadRequest] =
    DeriveJsonEncoder.gen[BadRequest]

  implicit val jsonDecoder_conflict: JsonDecoder[Conflict] =
    DeriveJsonDecoder.gen[Conflict]
  implicit val jsonEncoder_conflict: JsonEncoder[Conflict] =
    DeriveJsonEncoder.gen[Conflict]

  implicit val jsonDecoder_notFound: JsonDecoder[NotFound] =
    DeriveJsonDecoder.gen[NotFound]
  implicit val jsonEncoder_notFound: JsonEncoder[NotFound] =
    DeriveJsonEncoder.gen[NotFound]

  implicit val jsonDecoder_unauthorized: JsonDecoder[Unauthorized] =
    DeriveJsonDecoder.gen[Unauthorized]
  implicit val jsonEncoder_unauthorized: JsonEncoder[Unauthorized] =
    DeriveJsonEncoder.gen[Unauthorized]
}

object HttpErrorProtocol extends HttpErrorProtocol
