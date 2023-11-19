package zio.openai.codegen.model

import zio.openai.codegen.generator.Types
import scala.meta._

sealed trait ContentType {
  def asString: String = this match {
    case ContentType.`application/json`         => "application/json"
    case ContentType.`multipart/form-data`      => "multipart/form-data"
    case ContentType.`application/octet-stream` => "application/octet-stream"
  }

  def asMediaType =
    this match {
      case ContentType.`application/json`         =>
        q"${Types.zhttpMediaType.term}.application.json"
      case ContentType.`multipart/form-data`      =>
        q"${Types.zhttpMediaType.term}.multipart.`form-data`"
      case ContentType.`application/octet-stream` =>
        q"${Types.zhttpMediaType.term}.application.`octet-stream`"
    }
}

object ContentType {
  case object `application/json` extends ContentType
  case object `multipart/form-data` extends ContentType
  case object `application/octet-stream` extends ContentType

  def from(name: String): ContentType = name match {
    case "application/json"         => `application/json`
    case "multipart/form-data"      => `multipart/form-data`
    case "application/octet-stream" => `application/octet-stream`
    case _                          => throw new IllegalArgumentException(s"Unsupported content type: $name")
  }
}
