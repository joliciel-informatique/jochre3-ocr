package com.joliciel.jochre.ocr.api

import com.joliciel.jochre.ocr.api.HttpError.{BadRequest, InternalServerError, NotFound}

trait HttpErrorMapper {
  def mapToHttpError(exception: Throwable): HttpError = exception match {
    case e: NotFoundException => NotFound(e.getMessage)
    case e: BadRequestException => BadRequest(e.getMessage)
    case error: Throwable => InternalServerError(message = error.getMessage, cause = Some(error))
  }
}
