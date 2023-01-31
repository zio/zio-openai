package zio.openai.internal

import zio.{ Chunk, ZIO, ZOutputStream }
import zio.constraintless.{ IsElementOf, TypeList }
import zio.http.{ Body, Response }
import zio.openai.model.File
import zio.schema.{ Schema, StandardType }
import zio.schema.codec.BinaryCodecs

import java.io.{ ByteArrayOutputStream, DataOutputStream, OutputStream, OutputStreamWriter, Writer }
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

  def toMultipartFormDataBody[T: Schema](value: T, boundary: String): Either[String, Body] = {
    val buffer = new ByteArrayOutputStream()
    val output = new DataOutputStream(buffer)
    try {
      toMultipartFormDataBody(Schema[T], value, boundary, Chunk.empty, output)
      output.flush()
      val bytes = Chunk.fromArray(buffer.toByteArray)
      Right(Body.fromChunk(bytes))
    } catch {
      case NonFatal(err) =>
        Left(err.getMessage)
    }
  }

  private def toMultipartFormDataBody[T](
    schema: Schema[T],
    value: T,
    boundary: String,
    header: Chunk[Byte],
    output: DataOutputStream
  ): Unit = {
    def writeString(s: String): Unit = {
      output.write(header.toArray)
      output.write(s.getBytes(StandardCharsets.UTF_8))
    }

    schema match {
      case _ if schema == File.schema                    =>
        output.write(header.toArray)
        output.write(value.asInstanceOf[File].data.toArray)
      case _: Schema.Enum[_]                             =>
        throw new IllegalArgumentException("Cannot encode enum as multipart/form-data")
      case record: Schema.Record[_]                      =>
        if (header.isEmpty) {
          for ((field, idx) <- record.fields.zipWithIndex) {
            val headerBuilder = new StringBuilder()
            headerBuilder.append("--")
            headerBuilder.append(boundary)
            headerBuilder.append('\n')
            headerBuilder.append("Content-Disposition: form-data; name=\"")
            headerBuilder.append(field.name)
            headerBuilder.append("\"")
            if (field.schema == File.schema) {
              headerBuilder.append(
                s"""; filename="${field.get(value).asInstanceOf[File].fileName}""""
              )
            }
            headerBuilder.append("\n\n")
            val header = Chunk.fromArray(headerBuilder.toString().getBytes(StandardCharsets.UTF_8))

            toMultipartFormDataBody(
              field.schema.asInstanceOf[Schema[Any]],
              field.get(value),
              boundary,
              header,
              output
            )
            if (idx < (record.fields.length - 1))
              output.write('\n')
          }
          writeString("--")
          writeString(boundary)
          writeString("--")
        } else {
          throw new IllegalArgumentException("Cannot encode nested record as multipart/form-data")
        }
      case Schema.Sequence(elemSchema, _, toChunk, _, _) =>
        elemSchema.asInstanceOf[Schema[_]] match {
          case Schema.Primitive(StandardType.ByteType, _) =>
            val data = toChunk(value).asInstanceOf[Chunk[Byte]]
            output.write(header.toArray)
            output.write(data.toArray)
          case _                                          =>
            throw new IllegalArgumentException("Cannot encode collections as multipart/form-data")
        }
      case _: Schema.Collection[_, _]                    =>
        throw new IllegalArgumentException("Cannot encode collections as multipart/form-data")
      case Schema.Transform(schema, _, g, _, _)          =>
        g(value).map { inner =>
          toMultipartFormDataBody(schema, inner, boundary, header, output)
        }
      case Schema.Primitive(standardType, _)             =>
        standardType match {
          case StandardType.UnitType   =>
          case StandardType.StringType =>
            writeString(value.asInstanceOf[String])
          case StandardType.BoolType   =>
            if (value.asInstanceOf[Boolean])
              writeString("true")
            else
              writeString("false")
          case StandardType.BinaryType =>
            val bytes = value.asInstanceOf[Chunk[Byte]]
            output.write(header.toArray)
            output.write(bytes.toArray)
          case _                       =>
            writeString(value.toString)
        }
      case Schema.Optional(inner, _)                     =>
        value.asInstanceOf[Option[Any]] match {
          case Some(innerValue) =>
            toMultipartFormDataBody[Any](
              inner.asInstanceOf[Schema[Any]],
              innerValue,
              boundary,
              header,
              output
            )
          case None             =>
        }

      case Schema.Fail(message, annotations) =>
        throw new IllegalStateException(message)
      case Schema.Tuple2(_, _, _)            =>
        throw new IllegalArgumentException("Cannot encode tuple as multipart/form-data")
      case Schema.Either(_, _, _)            =>
        throw new IllegalArgumentException("Cannot encode either as multipart/form-data")
      case Schema.Lazy(schema0)              =>
        toMultipartFormDataBody(schema0(), value, boundary, header, output)
      case Schema.Dynamic(_)                 =>
        throw new IllegalArgumentException("Cannot encode dynamic as multipart/form-data")
    }
  }
}
