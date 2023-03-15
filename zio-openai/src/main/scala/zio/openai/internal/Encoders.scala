package zio.openai.internal

import zio.Chunk
import zio.constraintless.{ IsElementOf, TypeList }
import zio.http.Body
import zio.http.forms.{ Form, FormData }
import zio.http.model.MediaType
import zio.openai.model.File
import zio.prelude._
import zio.schema.codec.BinaryCodecs
import zio.schema.{ Schema, StandardType }

import java.io.{ ByteArrayOutputStream, DataOutputStream }
import java.nio.charset.StandardCharsets
import scala.util.control.NonFatal

object Encoders {
  def toJsonBody[T]: ToJsonBody[T] = new ToJsonBody[T](())
  def toURLSegment[T](value: T)(implicit encoder: URLSegmentEncoder[T]): String =
    encoder.encode(value)

  class ToJsonBody[T](val unit: Unit) extends AnyVal {
    def apply[Types <: TypeList](codecs: BinaryCodecs[Types], value: T)(implicit
      ev: T IsElementOf Types
    ): Body =
      Body.fromChunk(codecs.encode(value))
  }

  trait URLSegmentEncoder[T] {
    def encode(value: T): String
  }

  object URLSegmentEncoder {
    implicit lazy val encodeString: URLSegmentEncoder[String] = (value: String) => value
    implicit lazy val encodeInt: URLSegmentEncoder[Int] = (value: Int) => value.toString
    implicit lazy val encodeLong: URLSegmentEncoder[Long] = (value: Long) => value.toString
    implicit lazy val encodeDouble: URLSegmentEncoder[Double] = (value: Double) => value.toString
    implicit lazy val encodeFloat: URLSegmentEncoder[Float] = (value: Float) => value.toString
    implicit lazy val encodeBoolean: URLSegmentEncoder[Boolean] = (value: Boolean) => value.toString
    implicit lazy val encodeUnit: URLSegmentEncoder[Unit] = (_: Unit) => ""
  }

  def toMultipartFormDataBody[T: Schema](value: T, boundary: String): Either[String, Body] =
    toMultipartFormDataBody(Schema[T], value, None).map { formData =>
      // TODO: boundary cannot be passed to zio-http yet, fix it when possible
      Body.fromMultipartForm(Form(formData))
    }

  private def toMultipartFormDataBody[T](
    schema: Schema[T],
    value: T,
    fieldName: Option[String]
  ): Either[String, Chunk[FormData]] =
    schema match {
      case _ if schema == File.schema                    =>
        val file = value.asInstanceOf[File]
        Right(
          Chunk(
            FormData.binaryField(
              fieldName.getOrElse("unknown"),
              file.data,
              MediaType.application.`octet-stream`,
              transferEncoding = None,
              Some(file.fileName)
            )
          )
        )
      case _: Schema.Enum[_]                             =>
        Left("Cannot encode enum as multipart/form-data")
      case record: Schema.Record[_]                      =>
        if (fieldName.isEmpty) {
          record.fields
            .forEach { field =>
              toMultipartFormDataBody[Any](
                field.schema.asInstanceOf[Schema[Any]],
                field.get(value),
                Some(field.name)
              )
            }
            .map(_.flatten)
        } else {
          Left("Cannot encode nested record as multipart/form-data")
        }
      case Schema.Sequence(elemSchema, _, toChunk, _, _) =>
        elemSchema.asInstanceOf[Schema[_]] match {
          case Schema.Primitive(StandardType.ByteType, _) =>
            Right(
              Chunk(
                FormData.binaryField(
                  fieldName.getOrElse("unknown"),
                  toChunk(value).asInstanceOf[Chunk[Byte]],
                  MediaType.application.`octet-stream`
                )
              )
            )
          case _                                          =>
            Left("Cannot encode collections as multipart/form-data")
        }
      case _: Schema.Collection[_, _]                    =>
        Left("Cannot encode collections as multipart/form-data")
      case Schema.Transform(schema, _, g, _, _)          =>
        g(value).flatMap { inner =>
          toMultipartFormDataBody(schema, inner, fieldName)
        }
      case Schema.Primitive(standardType, _)             =>
        standardType match {
          case StandardType.UnitType   => Right(Chunk.empty)
          case StandardType.StringType =>
            Right(
              Chunk(
                FormData.simpleField(fieldName.getOrElse("unknown"), value.asInstanceOf[String])
              )
            )
          case StandardType.BoolType   =>
            Right(
              Chunk(
                if (value.asInstanceOf[Boolean])
                  FormData.simpleField(fieldName.getOrElse("unknown"), "true")
                else
                  FormData.simpleField(fieldName.getOrElse("unknown"), "true")
              )
            )
          case StandardType.BinaryType =>
            Right(
              Chunk(
                FormData.binaryField(
                  fieldName.getOrElse("unknown"),
                  value.asInstanceOf[Chunk[Byte]],
                  MediaType.application.`octet-stream`
                )
              )
            )
          case _                       =>
            Right(
              Chunk(
                FormData.simpleField(fieldName.getOrElse("unknown"), value.toString)
              )
            )
        }
      case Schema.Optional(inner, _)                     =>
        value.asInstanceOf[Option[Any]] match {
          case Some(innerValue) =>
            toMultipartFormDataBody[Any](
              inner,
              innerValue,
              fieldName
            )
          case None             =>
            Right(Chunk.empty)
        }

      case Schema.Fail(message, _) =>
        Left(message)
      case Schema.Tuple2(_, _, _)  =>
        Left("Cannot encode tuple as multipart/form-data")
      case Schema.Either(_, _, _)  =>
        Left("Cannot encode either as multipart/form-data")
      case Schema.Lazy(schema0)    =>
        toMultipartFormDataBody(schema0(), value, fieldName)
      case Schema.Dynamic(_)       =>
        Left("Cannot encode dynamic as multipart/form-data")
    }
}
