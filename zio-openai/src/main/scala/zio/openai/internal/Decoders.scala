package zio.openai.internal

import zio.ZIO
import zio.constraintless.{ IsElementOf, TypeList }
import zio.http.Response
import zio.http.model.Status
import zio.schema.codec.BinaryCodecs

private[openai] object Decoders {

  // TODO: have request info to be included in errors
  // TODO: typed error
  def tryDecodeJsonResponse[T]: TryDecodeJsonResponse[T] = new TryDecodeJsonResponse[T](())

  def validateEmptyResponse(response: Response): ZIO[Any, Throwable, Unit] =
    ZIO
      .fail(new RuntimeException(s"Request returned with ${response.status}"))
      .unless(response.status == Status.Ok)
      .unit

  class TryDecodeJsonResponse[T](val unit: Unit) extends AnyVal {
    def apply[Types <: TypeList](codecs: BinaryCodecs[Types], response: Response)(implicit
      ev: T IsElementOf Types
    ): ZIO[Any, Throwable, T] =
      if (response.status == Status.Ok) {
        response.body.asChunk.flatMap { bytes =>
          ZIO.fromEither(codecs.decode[T](bytes)).mapError { decodeError =>
            new RuntimeException(s"Failed to decode response: $decodeError")
          }
        }
      } else {
        // TODO
        response.body.asString.flatMap { body =>
          ZIO.fail(new RuntimeException(s"Request returned with ${response.status}: $body"))
        }
      }
  }
}
