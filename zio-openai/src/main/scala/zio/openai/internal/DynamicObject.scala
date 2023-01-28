package zio.openai.internal

import zio.json.ast.Json

import scala.language.dynamics

abstract class DynamicObject[Self] extends Dynamic {
  val values: Map[String, Json]

  protected def updateValues(updated: Map[String, Json]): Self

  def selectDynamic(name: String): Json = values(name)

  def applyDynamicNamed(name: String)(args: (String, Json)*): Self = {
    if (name == "update") {
      val updated = args.toMap
      updateValues(values ++ updated)
    } else {
      throw new IllegalArgumentException(s"Unknown method $name")
    }
  }
}

