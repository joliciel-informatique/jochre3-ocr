package com.joliciel.jochre.ocr.api

sealed trait HttpError

object HttpError {
  case class InternalServerError(message: String, cause: Option[Throwable]) extends HttpError
  case class BadRequest(message: String) extends HttpError
  case class NotFound(message: String) extends HttpError
  case class Conflict(message: String) extends HttpError
  case class Unauthorized(message: String) extends HttpError
}
