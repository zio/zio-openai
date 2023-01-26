package zio.openai.codegen.model

import io.github.vigoo.metagen.core.ScalaType
import zio.openai.codegen.generator.Types

import scala.meta.Term

sealed trait HttpMethod {
  def constructor: ScalaType =
    this match {
      case HttpMethod.Options => Types.zhttpMethod / "OPTIONS"
      case HttpMethod.Get     => Types.zhttpMethod / "GET"
      case HttpMethod.Put     => Types.zhttpMethod / "PUT"
      case HttpMethod.Delete  => Types.zhttpMethod / "DELETE"
      case HttpMethod.Head    => Types.zhttpMethod / "HEAD"
      case HttpMethod.Patch   => Types.zhttpMethod / "PATCH"
      case HttpMethod.Post    => Types.zhttpMethod / "POST"
      case HttpMethod.Trace   => Types.zhttpMethod / "TRACE"
    }
}
object HttpMethod {
  case object Options extends HttpMethod
  case object Get extends HttpMethod
  case object Put extends HttpMethod
  case object Delete extends HttpMethod
  case object Head extends HttpMethod
  case object Patch extends HttpMethod
  case object Post extends HttpMethod
  case object Trace extends HttpMethod

}
