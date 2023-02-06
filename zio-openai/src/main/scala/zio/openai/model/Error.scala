package zio.openai.model

import zio.schema.{ DeriveSchema, Schema }

final case class Error(message: String, `type`: String, param: Option[String], code: Option[String])

object Error {
  implicit val schema: Schema[Error] = DeriveSchema.gen[Error]
}
