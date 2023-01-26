package zio.openai.codegen.generator

import io.github.vigoo.metagen.core.GeneratorFailure
import io.swagger.parser.OpenAPIParser
import io.swagger.v3.parser.core.models.ParseOptions
import zio.ZIO
import zio.nio.file.{Files, Path}
import zio.openai.codegen.model.Model

import java.nio.charset.StandardCharsets
import scala.jdk.CollectionConverters.*

object Loader {
  def loadModel(openAPIPath: Path): ZIO[Any, GeneratorFailure[OpenAIGeneratorFailure], Model] = {
    for {
      bytes        <- Files.readAllBytes(openAPIPath)
      rawYaml       = new String(bytes.toArray, StandardCharsets.UTF_8)
      parser        = new OpenAPIParser
      opts         <- ZIO.attempt {
                        val opts = new ParseOptions()
                        opts.setResolve(true)
                        opts
                      }
      parserResult <- ZIO.attempt(parser.readContents(rawYaml, List.empty.asJava, opts))
      _            <- ZIO.foreachDiscard(Option(parserResult.getMessages.asScala).getOrElse(Nil)) { msg =>
                        ZIO.logInfo(msg)
                      }
      openAPI      <- Option(parserResult.getOpenAPI) match {
                        case Some(value) => ZIO.succeed(value)
                        case None        =>
                          ZIO.fail(new RuntimeException("Failed to parse OpenAPI"))
                      }
      model         = Model.from(openAPI)
    } yield model
  }.mapError(e => GeneratorFailure.CustomFailure(OpenAIGeneratorFailure.FailedToLoadOpenAPI(e)))
}
