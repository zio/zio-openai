package zio.openai.codegen.generator

sealed trait OpenAIGeneratorFailure

object OpenAIGeneratorFailure {
  final case class FailedToLoadOpenAPI(reason: Throwable) extends OpenAIGeneratorFailure
}
