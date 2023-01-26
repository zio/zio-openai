package zio.openai.codegen.model

import io.swagger.v3.oas.models.{ Operation, PathItem }

import scala.jdk.CollectionConverters.*

final case class API(name: String, endpoints: List[Endpoint])

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
            val group = meta.asScala.toMap.asInstanceOf[Map[String, String]].getOrElse("group", "")
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
          group,
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
                TypeDefinition.from(s"${group}_${op.getOperationId}", contentSpec.getSchema)
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
                    .from(s"${group}_${op.getOperationId}_response", contentSpec.getSchema)
                )
              }

            Endpoint(
              op.getOperationId,
              method,
              path,
              Option(op.getDeprecated).exists(_.booleanValue()),
              parameters,
              body,
              response
            )
          }.toList
        )
      }
      .toList
  }
}
