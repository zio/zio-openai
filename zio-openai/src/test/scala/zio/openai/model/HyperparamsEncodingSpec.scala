package zio.openai.model

import zio.{ Chunk, Scope }
import zio.json.ast.Json
import zio.openai.model.FineTune.Hyperparams
import zio.prelude.data.Optional
import zio.schema.codec.JsonCodec
import zio.test.{ assertTrue, Spec, TestEnvironment, ZIOSpecDefault }

import java.nio.charset.StandardCharsets

object HyperparamsEncodingSpec extends ZIOSpecDefault {
  override def spec: Spec[TestEnvironment with Scope, Any] =
    suite("Hyperparams")(
      test("is encoded to JSON as expected") {
        val hyperparams =
          Hyperparams(batchSize = 5, promptLossWeight = 0.1).update(additional = Json.Num(10))
        val bytes = JsonCodec.schemaBasedBinaryCodec[Hyperparams].encode(hyperparams)
        val str = new String(bytes.toArray)
        assertTrue(str == """{"batch_size":5,"prompt_loss_weight":0.1,"additional":10}""")
      },
      test("can be decoded from JSON") {
        val str = """{"batch_size":5,"prompt_loss_weight":0.1,"additional":10}"""
        val bytes = Chunk.fromArray(str.getBytes(StandardCharsets.UTF_8))
        val hyperparams = JsonCodec.schemaBasedBinaryCodec[Hyperparams].decode(bytes).toOption.get
        assertTrue(
          hyperparams.batchSize == Optional.Present(5),
          hyperparams.promptLossWeight == Optional.Present(0.1),
          hyperparams.additional == Json.Num(10)
        )
      }
    )
}
