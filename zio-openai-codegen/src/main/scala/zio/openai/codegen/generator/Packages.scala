package zio.openai.codegen.generator

import io.github.vigoo.metagen.core.*

object Packages {

  val zio: Package = Package("zio")
  val zioConstraintless: Package = zio / "constraintless"
  val zioHttp: Package = zio / "http"
  val zioJson: Package = zio / "json"
  val zioJsonAst: Package = zioJson / "ast"
  val zioPrelude: Package = zio / "prelude"
  val zioSchema: Package = zio / "schema"
  val zioSchemaAnnotation: Package = zioSchema / "annotation"
  val zioSchemaCodec: Package = zioSchema / "codec"
  val zioStream: Package = zio / "stream"

  val openai: Package = zio / "openai"
  val internal: Package = openai / "internal"
  val models: Package = openai / "model"
}
