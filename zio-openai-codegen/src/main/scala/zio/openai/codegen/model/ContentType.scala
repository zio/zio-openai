package zio.openai.codegen.model

sealed trait ContentType {
  def asString: String = this match {
    case ContentType.`application/json`    => "application/json"
    case ContentType.`multipart/form-data` => "multipart/form-data"
  }
}

object ContentType {
  case object `application/json` extends ContentType
  case object `multipart/form-data` extends ContentType

  def from(name: String): ContentType = name match {
    case "application/json"    => `application/json`
    case "multipart/form-data" => `multipart/form-data`
    case _                     => throw new IllegalArgumentException(s"Unsupported content type: $name")
  }
}
