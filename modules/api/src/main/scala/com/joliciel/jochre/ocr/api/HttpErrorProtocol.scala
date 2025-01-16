package com.joliciel.jochre.ocr.api

import com.joliciel.jochre.ocr.api.HttpError.{BadRequest, Conflict, NotFound, Unauthorized}
import zio.json._

trait HttpErrorProtocol {
  given JsonDecoder[BadRequest] = DeriveJsonDecoder.gen[BadRequest]
  given JsonEncoder[BadRequest] = DeriveJsonEncoder.gen[BadRequest]

  given JsonDecoder[Conflict] = DeriveJsonDecoder.gen[Conflict]
  given JsonEncoder[Conflict] = DeriveJsonEncoder.gen[Conflict]

  given JsonDecoder[NotFound] = DeriveJsonDecoder.gen[NotFound]
  given JsonEncoder[NotFound] = DeriveJsonEncoder.gen[NotFound]

  given JsonDecoder[Unauthorized] = DeriveJsonDecoder.gen[Unauthorized]
  given JsonEncoder[Unauthorized] = DeriveJsonEncoder.gen[Unauthorized]
}

object HttpErrorProtocol extends HttpErrorProtocol
