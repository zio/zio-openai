package zio.openai.codegen.generator

final case class OpenAIGeneratorImpl(parameters: Parameters)
    extends OpenAIGenerator with APIGenerator with ModelGenerator with HasParameters {

}
