package zio.openai.codegen.model

final case class RequestBody(contentType: ContentType, typ: TypeDefinition) {
  def transform(f: TypeDefinition => TypeDefinition): RequestBody =
    copy(typ = typ.transform(f))
}
