package zio.openai.codegen.model

final case class ResponseBody(contentType: ContentType, typ: TypeDefinition) {
  def transformEnums(f: TypeDefinition.Enum => TypeDefinition.Enum): ResponseBody =
    copy(typ = typ.transformEnums(f))
}
