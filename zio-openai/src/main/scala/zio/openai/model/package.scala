package zio.openai

import zio.{ Chunk, NonEmptyChunk }
import zio.schema.Schema

package object model {

  implicit def nonEmptyChunkSchema[A: Schema]: Schema[NonEmptyChunk[A]] =
    Schema
      .chunk[A]
      .transformOrFail(
        (chunk: Chunk[A]) =>
          chunk
            .nonEmptyOrElse[Either[String, NonEmptyChunk[A]]](Left("Must be non-empty"))(Right(_)),
        (nonEmptyChunk: NonEmptyChunk[A]) => Right(nonEmptyChunk.toChunk)
      )
}
