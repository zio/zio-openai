package zio.openai.examples

import zio.json.ast.Json
import zio.openai.Embeddings
import zio.openai.model.CreateEmbeddingRequest.Model.CaseType1
import zio.openai.model.CreateEmbeddingRequest.{Input, Model}
import zio.{Console, Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

/** Based on https://beta.openai.com/docs/api-reference/embeddings
  */
object Embedding extends ZIOAppDefault {

  def createEmbedding =
    for {
      response <- Embeddings.createEmbedding(
                    model = Model.Case1(CaseType1.`Text-embedding-ada-002`),
                    input = Input.String("The food was delicious and the waiter...")
                  )
      _        <- Console.printLine(response.data.toString)
    } yield ()

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    createEmbedding.provide(Embeddings.default)
}
