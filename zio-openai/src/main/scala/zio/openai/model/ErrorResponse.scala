package zio.openai.model

import zio.schema.codec.{ BinaryCodec, JsonCodec }
import zio.schema.{ DeriveSchema, Schema }

final case class ErrorResponse(error: Error)

object ErrorResponse {
  implicit val schema: Schema[ErrorResponse] = DeriveSchema.gen[ErrorResponse]
  val jsonCodec: BinaryCodec[ErrorResponse] = JsonCodec.schemaBasedBinaryCodec[ErrorResponse]
}
