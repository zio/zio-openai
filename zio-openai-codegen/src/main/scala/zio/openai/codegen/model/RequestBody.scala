package zio.openai.codegen.model

final case class RequestBody(contentType: ContentType, typ: TypeDefinition) {
  def transformEnums(f: TypeDefinition.Enum => TypeDefinition.Enum): RequestBody =
    copy(typ = typ.transformEnums(f))
}
