package zio.openai.codegen.model

final case class ResponseBody(contentType: ContentType, typ: TypeDefinition) {
  def transform(f: TypeDefinition => TypeDefinition): ResponseBody =
    copy(typ = typ.transform(f))
}
