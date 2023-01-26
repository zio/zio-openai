package zio.openai.codegen.generator

import io.github.vigoo.metagen.core.GeneratorFailure
import zio.nio.file.Path
import zio.openai.codegen.model.Model
import zio.{ ZIO, ZLayer }

trait OpenAIGenerator {
  def generateModels(model: Model): ZIO[Any, GeneratorFailure[OpenAIGeneratorFailure], Set[Path]]
  def generateServices(model: Model): ZIO[Any, GeneratorFailure[OpenAIGeneratorFailure], Set[Path]]
}

object OpenAIGenerator {

  def generateModels(
    model: Model
  ): ZIO[OpenAIGenerator, GeneratorFailure[OpenAIGeneratorFailure], Set[Path]] =
    ZIO.serviceWithZIO(_.generateModels(model))

  def generateServices(
    model: Model
  ): ZIO[OpenAIGenerator, GeneratorFailure[OpenAIGeneratorFailure], Set[Path]] =
    ZIO.serviceWithZIO(_.generateServices(model))

  val live: ZLayer[Parameters, Nothing, OpenAIGeneratorImpl] =
    ZLayer {
      for {
        parameters <- ZIO.service[Parameters]
      } yield OpenAIGeneratorImpl(parameters)
    }
}
