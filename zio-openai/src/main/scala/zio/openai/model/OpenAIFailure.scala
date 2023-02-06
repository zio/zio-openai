package zio.openai.model

import zio.Chunk
import zio.http.URL
import zio.http.model.{ Method, Status }
import zio.schema.codec.DecodeError

sealed trait OpenAIFailure

object OpenAIFailure {
  final case class ErrorResponse(url: URL, method: Method, code: Status, error: Error)
      extends OpenAIFailure

  final case class UnknownErrorResponse(
    url: URL,
    method: Method,
    code: Status,
    error: String,
    decodeError: DecodeError
  ) extends OpenAIFailure

  final case class ResponseDecodeError(
    url: URL,
    method: Method,
    decodeError: DecodeError,
    rawResponse: String
  ) extends OpenAIFailure

  final case class Unknown(failure: Throwable) extends OpenAIFailure

  final case class EncodingError(reason: String) extends OpenAIFailure
}
