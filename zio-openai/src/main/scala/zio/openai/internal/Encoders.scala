package zio.openai.internal

import zio.ZIO
import zio.constraintless.{ IsElementOf, TypeList }
import zio.http.{ Body, Response }
import zio.schema.codec.BinaryCodecs

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
}
