package zio.openai.internal

import zio.ZIO
import zio.constraintless.{IsElementOf, TypeList}
import zio.http.Status
import zio.http.{Request, Response}
import zio.openai.model.{ErrorResponse, OpenAIFailure}
import zio.schema.codec.{BinaryCodecs, JsonCodec}

import java.nio.charset.StandardCharsets

private[openai] object Decoders {

  def tryDecodeJsonResponse[T]: TryDecodeJsonResponse[T] = new TryDecodeJsonResponse[T](())

  def validateEmptyResponse(request: Request, response: Response): ZIO[Any, OpenAIFailure, Unit] =
    failWithErrorResponse(request, response).unless(response.status == Status.Ok).unit

  class TryDecodeJsonResponse[T](val unit: Unit) extends AnyVal {
    def apply[Types <: TypeList](codecs: BinaryCodecs[Types], request: Request, response: Response)(
      implicit ev: T IsElementOf Types
    ): ZIO[Any, OpenAIFailure, T] =
      if (response.status == Status.Ok) {
        response.body.asChunk
          .mapError(OpenAIFailure.Unknown(_))
          .flatMap { bytes =>
            ZIO.fromEither(codecs.decode[T](bytes)).mapError { decodeError =>
              OpenAIFailure.ResponseDecodeError(
                request.url,
                request.method,
                decodeError,
                new String(bytes.toArray, StandardCharsets.UTF_8)
              )
            }
          }
      } else {
        failWithErrorResponse(request, response)
      }
  }

  private val errorResponseCodec = JsonCodec.schemaBasedBinaryCodec(ErrorResponse.schema)

  private def failWithErrorResponse(
    request: Request,
    response: Response
  ): ZIO[Any, OpenAIFailure, Nothing] =
    response.body.asChunk
      .mapError(OpenAIFailure.Unknown(_))
      .flatMap { body =>
        ZIO
          .fromEither(errorResponseCodec.decode(body))
          .catchAll { msg =>
            val unknownError = new String(body.toArray, StandardCharsets.UTF_8)
            ZIO.fail(
              OpenAIFailure.UnknownErrorResponse(
                request.url,
                request.method,
                response.status,
                unknownError,
                msg
              )
            )
          }
          .flatMap { typedErrorResponse =>
            val typedError = typedErrorResponse.error
            ZIO.fail(
              OpenAIFailure.ErrorResponse(
                request.url,
                request.method,
                response.status,
                typedError
              )
            )
          }
      }
}
