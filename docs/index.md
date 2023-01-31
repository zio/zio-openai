---
id: index
title: "Introduction to ZIO OpenAI"
sidebar_label: "ZIO OpenAI"
---

Library for using the [OpenAI API](https://beta.openai.com/docs/introduction/overview)

@PROJECT_BADGES@

## Introduction

This library provides Scala data types and ZIO services for using the OpenAI API.
The [examples](tree/main/zio-openai-examples/src/main/scala/zio/openai/examples) directory contains a
few examples
of how to use the different features of the library.

The following example is the translation of [OpenAI's official quickstart example](https://beta.openai.com/docs/quickstart):

```scala mdoc
import zio.{Console, ZIO, ZIOAppDefault}
import zio.openai._
import zio.openai.model.CreateCompletionRequest.Prompt
import zio.openai.model.Temperature

object Quickstart extends ZIOAppDefault {

  def generatePrompt(animal: String): Prompt =
    Prompt.String {
      s"""Suggest three names for an animal that is a superhero.
         |
         |Animal: Cat
         |Names: Captain Sharpclaw, Agent Fluffball, The Incredible Feline
         |Animal: Dog
         |Names: Ruff the Protector, Wonder Canine, Sir Barks-a-Lot
         |Animal: ${animal.capitalize}
         |Names:""".stripMargin
    }

  def loop =
    for {
      animal <- Console.readLine("Animal: ")
      result <- Completions.createCompletion(
        model = "text-davinci-003",
        prompt = generatePrompt(animal),
        temperature = Temperature(0.6)
      )
      _ <- Console.printLine("Names: " + result.choices.flatMap(_.text.toOption).mkString(", "))
    } yield ()

  override def run =
    loop.forever.provide(Completions.default)
}
```

The `Completions.default` layer initializes the OpenAI client with the default zio-http client configuration and uses
ZIO's [built-in configuration
system](https://degoes.net/articles/zio-config) to get the OpenAI API key. The default configuration provider looks for
the API Key in the `OPENAI_APIKEY`
environment variable or the `openAI.apiKey` system property.

If your project is using `zio-http` for other purposes as well and you already have a `Client` layer set up, you can use
the `live` variants of the layers (`Completions.live`) to share the same client.

## Installation

Start by adding `zio-openai` as a dependency to your project:

```scala
libraryDependencies += "dev.zio" %% "zio-openai" % "<version>"
```

[//]: # (TODO: Add example section)

[//]: # (## Example)