package zio.openai.codegen.model

import io.swagger.v3.oas.models.{ Operation, PathItem }
import zio.openai.codegen.generator.Naming.{ toCamelCase, toPascalCase }

import scala.jdk.CollectionConverters.*

final case class API(name: String, endpoints: List[Endpoint]) {
  require(name.nonEmpty)

  def transform(f: TypeDefinition => TypeDefinition): API =
    copy(endpoints = endpoints.map(_.transform(f)))
}

object API {
  def fromPaths(paths: Map[String, PathItem]): List[API] = {
    val endpoints = paths.flatMap { case (pathString, path) =>
      val operations = List[Option[(HttpMethod, Operation)]](
        Option(path.getOptions).map((HttpMethod.Options, _)),
        Option(path.getGet).map((HttpMethod.Get, _)),
        Option(path.getPut).map((HttpMethod.Put, _)),
        Option(path.getDelete).map((HttpMethod.Delete, _)),
        Option(path.getHead).map((HttpMethod.Head, _)),
        Option(path.getPatch).map((HttpMethod.Patch, _)),
        Option(path.getPost).map((HttpMethod.Post, _)),
        Option(path.getTrace).map((HttpMethod.Trace, _))
      ).flatten
      operations.flatMap { case (method, op) =>
        Option(op.getExtensions)
          .map(_.asScala)
          .getOrElse(Map.empty[String, Any])
          .get("x-oaiMeta") match {
          case Some(meta: java.util.HashMap[?, ?]) =>
            val groupFromPath = pathString
              .split("/")
              .find(_.nonEmpty)
              .filter(s => s(0).isLetter)
              .getOrElse("other")
            val group =
              meta.asScala.toMap.asInstanceOf[Map[String, String]].getOrElse("group", groupFromPath)
            List((group, pathString, method, op))
          case _                                   =>
            println(s"$path/${op.getOperationId} has no x-oaiMeta")
            Nil
        }
      }
    }

    endpoints
      .groupBy(_._1)
      .map { case (group, endpoints) =>
        API(
          toPascalCase(group),
          endpoints.map { case (_, path, method, op) =>
            val parameterSpecs = Option(op.getParameters).map(_.asScala.toList).getOrElse(Nil)
            val parameters = parameterSpecs.map { spec =>
              Parameter.from(s"${group}_${op.getOperationId}", spec)
            }

            val requestBodySpec = Option(op.getRequestBody)
            val body = requestBodySpec.map { spec =>
              val contentTypes = Option(spec.getContent).map(_.asScala.toMap).getOrElse(Map.empty)
              if (contentTypes.size != 1)
                throw new IllegalArgumentException(
                  s"Expected exactly one content type for ${op.getOperationId}, got ${contentTypes.size}"
                )

              val (contentTypeName, contentSpec) = contentTypes.head
              val contentType = ContentType.from(contentTypeName)
              RequestBody(
                contentType,
                TypeDefinition.from(
                  ParentChain.empty,
                  s"${group}_${op.getOperationId}",
                  contentSpec.getSchema
                ) // TODO: grouping?
              )
            }

            val responses = Option(op.getResponses).map(_.asScala.toMap).getOrElse(Map.empty)
            val response =
              responses.get("200").map { spec =>
                val contentTypes = Option(spec.getContent).map(_.asScala.toMap).getOrElse(Map.empty)
                if (contentTypes.size != 1)
                  throw new IllegalArgumentException(
                    s"Expected exactly one content type for the result of ${op.getOperationId}, got ${contentTypes.size}"
                  )

                val (contentTypeName, contentSpec) = contentTypes.head
                val contentType = ContentType.from(contentTypeName)

                ResponseBody(
                  contentType,
                  TypeDefinition
                    .from(
                      ParentChain.empty,
                      s"${group}_${op.getOperationId}_response", // TODO: grouping?
                      contentSpec.getSchema
                    )
                )
              }

            Endpoint(
              toCamelCase(op.getOperationId),
              method,
              path,
              Option(op.getDeprecated).exists(_.booleanValue()),
              parameters,
              body,
              response,
              Option(op.getSummary)
            )
          }.toList
        )
      }
      .toList
  }
}
