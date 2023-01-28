package zio.openai

import zio.{Chunk, Config}
import zio.Config.Secret
import zio.http.URL

import java.net.URI

final case class OpenAIConfig(baseURL: URL, apiKey: Secret)

object OpenAIConfig {
  val config: Config[OpenAIConfig] =
    (Config.uri("baseURL").withDefault(new URI("https://api.openai.com/v1")) ++
      Config.secret("apiKey"))
      .mapOrFail { case (uri, apiKey) =>
        URL
          .fromString(uri.toString)
          .map { baseURL =>
            OpenAIConfig(baseURL, apiKey)
          }
          .left
          .map { exception =>
            Config.Error.InvalidData(Chunk.empty, exception.getMessage)
          }
      }
      .nested("openai")
}
