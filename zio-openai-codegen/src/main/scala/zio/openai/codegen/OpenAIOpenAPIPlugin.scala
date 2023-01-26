package zio.openai.codegen

import sbt.Keys.*
import sbt.*
import zio.openai.codegen.OpenAIOpenAPIPlugin.autoImport.getOpenAIOpenAPI

import scala.sys.process.*
import scala.language.postfixOps

object OpenAIOpenAPIPlugin extends AutoPlugin {
  object autoImport {

    lazy val openAIVersion = settingKey[String]("OpenAI version")
    lazy val getOpenAIOpenAPI = taskKey[File]("Downloads the OpenAI OpenAPI definition")
  }

  import autoImport._

  override def projectSettings: Seq[Def.Setting[_]] =
    Seq(
      openAIVersion    := "master",
      getOpenAIOpenAPI := getOpenAIOpenAPITask.value
    )

  private lazy val getOpenAIOpenAPITask =
    Def.task {
      val ver = openAIVersion.value
      val targetDir = target.value / "openapi.yaml"
      val source =
        url(
          s"https://raw.githubusercontent.com/openai/openai-openapi/$ver/openapi.yaml"
        )
      source #> targetDir !

      targetDir
    }
}
