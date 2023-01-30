package zio.openai.examples

import zio.openai.Embeddings
import zio.openai.model.CreateEmbeddingRequest.Input
import zio.{ Console, Scope, ZIO, ZIOAppArgs, ZIOAppDefault }

/** Based on https://beta.openai.com/docs/api-reference/embeddings
  */
object Embedding extends ZIOAppDefault {

  def createEmbedding =
    for {
      response <- Embeddings.createEmbedding(
                    model = "text-embedding-ada-002",
                    input = Input.String("The food was delicious and the waiter...")
                  )
      _        <- Console.printLine(response.data.toString)
    } yield ()

  override def run: ZIO[ZIOAppArgs with Scope, Any, Any] =
    createEmbedding.provide(Embeddings.default)
}
