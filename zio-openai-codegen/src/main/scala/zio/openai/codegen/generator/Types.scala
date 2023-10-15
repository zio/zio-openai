package zio.openai.codegen.generator

import io.github.vigoo.metagen.core.{ Package, ScalaType }

object Types {
  def chunkOf(elem: ScalaType): ScalaType = ScalaType(Packages.zio, "Chunk", elem)
  val chunk_ : ScalaType = ScalaType(Packages.zio, "Chunk")

  val either_ : ScalaType = ScalaType(Package.scala / "util", "Either")
  val eitherLeft = ScalaType(Package.scala / "util", "Left")
  val eitherRight = ScalaType(Package.scala / "util", "Right")
  def nonEmptyChunkOf(elem: ScalaType): ScalaType = ScalaType(Packages.zio, "NonEmptyChunk", elem)
  val throwable: ScalaType = ScalaType(Package.javaLang, "Throwable")

  val caseSet: ScalaType = ScalaType(Packages.zioSchema, "CaseSet")
  val caseSetCons: ScalaType = caseSet / "Cons"
  val caseSetEmpty: ScalaType = caseSet / "Empty"

  val binaryCodecs: ScalaType = ScalaType(Packages.zioSchemaCodec, "BinaryCodecs")
  val noDiscriminator: ScalaType = ScalaType(Packages.zioSchemaAnnotation, "noDiscriminator")
  def schemaOf(typ: ScalaType): ScalaType = ScalaType(Packages.zioSchema, "Schema", typ)
  val schema_ : ScalaType = ScalaType(Packages.zioSchema, "Schema")
  def schemaCaseOf(parent: ScalaType, cons: ScalaType): ScalaType =
    ScalaType(Packages.zioSchema / "Schema", "Case", parent, cons)
  val schemaCase_ : ScalaType = schema_ / "Case"
  val schemaEnumN: ScalaType = schema_ / "EnumN"
  val schemaField: ScalaType = schema_ / "Field"
  val typeId: ScalaType = ScalaType(Packages.zioSchema, "TypeId")

  val secret: ScalaType = ScalaType(Packages.zio / "Config", "Secret")

  val json: ScalaType = ScalaType(Packages.zioJsonAst, "Json")

  val zhttpBody: ScalaType = ScalaType(Packages.zioHttp, "Body")
  val zhttpBoundary: ScalaType = ScalaType(Packages.zioHttp, "Boundary")
  val zhttpClient: ScalaType = ScalaType(Packages.zioHttp, "Client")
  val zhttpHeader: ScalaType = ScalaType(Packages.zioHttp, "Header")
  val zhttpMediaType: ScalaType = ScalaType(Packages.zioHttp, "MediaType")
  val zhttpMethod: ScalaType = ScalaType(Packages.zioHttp, "Method")
  val zhttpPath: ScalaType = ScalaType(Packages.zioHttp, "Path")
  val zhttpRequest: ScalaType = ScalaType(Packages.zioHttp, "Request")
  val zhttpURL: ScalaType = ScalaType(Packages.zioHttp, "URL")
  val zhttpQueryParams: ScalaType = ScalaType(Packages.zioHttp, "QueryParams")

  val random = ScalaType(Packages.zio, "Random")
  def zio(r: ScalaType, e: ScalaType, a: ScalaType): ScalaType =
    ScalaType(Packages.zio, "ZIO", r, e, a)
  val zio_ : ScalaType = ScalaType(Packages.zio, "ZIO")
  def zlayer(in: ScalaType, e: ScalaType, out: ScalaType): ScalaType =
    ScalaType(Packages.zio, "ZLayer", in, e, out)
  val zlayer_ : ScalaType = ScalaType(Packages.zio, "ZLayer")

  def zstream(r: ScalaType, e: ScalaType, a: ScalaType): ScalaType =
    ScalaType(Packages.zioStream, "ZStream", r, e, a)
  val zstream_ : ScalaType = ScalaType(Packages.zioStream, "ZStream")

  val typeListCons = ScalaType(Packages.zioConstraintless / "TypeList", "::")
  val typeListEnd = ScalaType(Packages.zioConstraintless / "TypeList", "End")

  def optional(t: ScalaType): ScalaType =
    ScalaType(Packages.zioPrelude / "data", "Optional", t)

  val optionalAbsent: ScalaType =
    ScalaType(Packages.zioPrelude / "data" / "Optional", "Absent")
  val optionalPresent: ScalaType =
    ScalaType(Packages.zioPrelude / "data" / "Optional", "Present")

  def subtypeOf(sup: ScalaType): ScalaType = ScalaType(Packages.zioPrelude, "Subtype", sup)

  val openAIConfig: ScalaType = ScalaType(Packages.openai, "OpenAIConfig")
  val decoders: ScalaType = ScalaType(Packages.internal, "Decoders")
  def dynamicObjectOf(self: ScalaType): ScalaType =
    ScalaType(Packages.internal, "DynamicObject", self)
  val encoders: ScalaType = ScalaType(Packages.internal, "Encoders")
  val file: ScalaType = ScalaType(Packages.models, "File")
  val openAIFailure: ScalaType = ScalaType(Packages.models, "OpenAIFailure")
}
