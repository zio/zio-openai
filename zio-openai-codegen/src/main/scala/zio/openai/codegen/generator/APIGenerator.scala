package zio.openai.codegen.generator

import io.github.vigoo.metagen.core.{ CodeFileGenerator, Generator, GeneratorFailure, ScalaType }
import zio.ZIO
import zio.nio.file.Path
import zio.openai.codegen.model.{ API, ContentType, Endpoint, Model, RequestBody, ResponseBody }

import scala.meta.*

// TODO: Proper casing for types and methods
// TODO: inline 'body' if it is the only parameter (make this an option)
// TODO: mark whole service as deprecated if all endpoints are deprecated (+option to not generate those)
// TODO: precalculate Paths when possible and store them in private vals

trait APIGenerator {
  this: HasParameters =>
  def generateServices(
    model: Model
  ): ZIO[Any, GeneratorFailure[OpenAIGeneratorFailure], Set[Path]] = {
    val generate =
      for {
        _        <- Generator.setRoot(parameters.targetRoot)
        _        <- Generator.setScalaVersion(parameters.scalaVersion)
        services <- ZIO.foreach(model.apis) { api =>
                      Generator.generateScalaPackage[Any, OpenAIGeneratorFailure](
                        Packages.openai,
                        api.name
                      )(generateApiClass(model, api))
                    }
      } yield services.toSet

    generate.provide(
      Generator.live
    )
  }

  // TODO: clean up (extract parts)
  private def generateApiClass(
    model: Model,
    api: API
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] = {
    val svc = ScalaType(Packages.openai, api.name)

    val interfaceMethods =
      api.endpoints.map { endpoint =>
        val paramList = getParamList(model, endpoint)
        val responseType = endpoint.responseType(model)
        val base: Decl.Def =
          q"def ${endpoint.methodName}(..$paramList): ${Types.zio(ScalaType.any, Types.throwable, responseType).typ}"
        if (endpoint.isDeprecated) base.copy(mods = Mod.Annot(init"deprecated()") :: base.mods)
        else base
      }

    val accessorMethods =
      api.endpoints.map { endpoint =>
        val paramList = getParamList(model, endpoint)
        val paramRefs = getParamRefs(endpoint)
        val responseType = endpoint.responseType(model)
        val base =
          q"""def ${endpoint.methodName}(..$paramList): ${Types
            .zio(svc, Types.throwable, responseType)
            .typ} =
              ${Types.zio_.term}.serviceWithZIO(_.${endpoint.methodName}(..$paramRefs))
         """

        if (endpoint.isDeprecated) base.copy(mods = Mod.Annot(init"deprecated()") :: base.mods)
        else base
      }

    val implMethods =
      api.endpoints.map { endpoint =>
        val paramList = getParamList(model, endpoint)
        val paramRefs = getParamRefs(endpoint)
        val responseType = endpoint.responseType(model)

        val queryParameters = endpoint.queryParameters
        val pathParameters = endpoint.pathParameters

        val basePathString = Lit.String(endpoint.pathPattern)
        val pathString =
          if (pathParameters.isEmpty)
            basePathString
          else
            pathParameters.foldLeft[Term](basePathString) { case (str, param) =>
              val litParamName = Lit.String(param.name)
              if (param.isRequired) {
                q"""
                  $str.replace(
                    $litParamName,
                    ${Types.encoders.term}.toURLSegment(${param.paramName})
                  )
                 """
              } else {
                q"""
                  $str.replace(
                    $litParamName,
                    ${param.paramName}.map(${Types.encoders.term}.toURLSegment).getOrElse("")
                  )
                 """
              }
            }
        val path = q"""${Types.zhttpPath.term}.decode($pathString)"""

        val hasQueryParams = queryParameters.nonEmpty
        val hasOptionalQueryParams = queryParameters.exists(!_.isRequired)

        val queryParams =
          if (hasOptionalQueryParams) {
            val optionalPairs = queryParameters.map { param =>
              q"""${param.paramName}.map { value => (${Lit.String(
                param.name
              )}, ${Types.encoders.term}.toURLSegment(value)) }"""
            }
            q"""${Types.zhttpQueryParams.term}(List(..$optionalPairs).flatten.map { case (k, v) => (k, ${Types.chunk_.term}(v)) }.toMap)"""
          } else {
            val pairs = queryParameters.map { param =>
              q"""(${Lit.String(
                param.name
              )}, ${Types.encoders.term}.toURLSegment(${param.paramName}))"""
            }
            q"""${Types.zhttpQueryParams.term}(..$pairs)"""
          }

        val url =
          if (hasQueryParams)
            q"""baseURL.setPath($path).setQueryParams($queryParams)"""
          else
            q"""baseURL.setPath($path)"""

        val body =
          endpoint.body match {
            case Some(RequestBody(ContentType.`application/json`, typ))    =>
              val bodyType = typ.scalaType(model)
              q"""${Types.zio_.term}.succeed(${Types.encoders.term}.toJsonBody[${bodyType.typ}](this.codecs, $bodyParam))"""
            case Some(RequestBody(ContentType.`multipart/form-data`, typ)) =>
              val bodyType = typ.scalaType(model)
              q"""${Types.zio_.term}.fromEither(${Types.encoders.term}.toMultipartFormDataBody[${bodyType.typ}]($bodyParam, this.boundary))
                      .mapError(new java.lang.RuntimeException(_))
               """
            case None                                                      =>
              q"""${Types.zio_.term}.succeed(${Types.zhttpBody.term}.empty)"""
          }

        val contentType =
          if (endpoint.body.exists(_.contentType == ContentType.`multipart/form-data`))
            q"""${Lit.String(endpoint.bodyContentTypeAsString)}+"; boundary="+this.boundary"""
          else
            Lit.String(endpoint.bodyContentTypeAsString)

        val request =
          q"""${Types.zhttpRequest.term}.default(
             method = ${endpoint.method.constructor.term},
             url = $url,
             body = body
           ).addHeader(${Types.zhttpHeaderNames.term}.authorization, authHeaderValue)
            .addHeader(${Types.zhttpHeaderNames.term}.contentType, $contentType)
           """

        val mapResponse =
          endpoint.response match {
            case Some(responseBody) if responseBody.contentType == ContentType.`application/json` =>
              val responseType = responseBody.typ.scalaType(model)
              q"""${Types.decoders.term}.tryDecodeJsonResponse[${responseType.typ}](this.codecs, response)"""
            case None                                                                             =>
              q"""${Types.decoders.term}.validateEmptyResponse(response)"""
            case _                                                                                =>
              throw new IllegalArgumentException(
                s"Unsupported response content type: ${endpoint.response}"
              )
          }

        val base =
          q"""def ${endpoint.methodName}(..$paramList): ${Types
            .zio(ScalaType.any, Types.throwable, responseType)
            .typ} =
                $body.flatMap { body =>
                  client.request($request).flatMap { response =>
                    $mapResponse
                  }
                }
          """

        if (endpoint.isDeprecated) base.copy(mods = Mod.Annot(init"deprecated()") :: base.mods)
        else base
      }

    val jsonTypes = api.endpoints.flatMap { endpoint =>
      endpoint.body.toList.collect { case RequestBody(ContentType.`application/json`, typ) =>
        typ
      } ++
        endpoint.response.toList.collect { case ResponseBody(ContentType.`application/json`, typ) =>
          typ
        }
    }

    val typeList = jsonTypes.foldRight[Type](Types.typeListEnd.typ) { (typ, lst) =>
      val scalaType = typ.scalaType(model)
      t"""${scalaType.typ} :: $lst"""
    }
    val codecs =
      q"""private val codecs: ${Types.binaryCodecs.typ}[$typeList] = {
            import _root_.zio.schema.codec.JsonCodec.schemaBasedBinaryCodec
            ${Types.binaryCodecs.term}.make
          }
        """

    val liveClass =
      q"""class Live(client: ${Types.zhttpClient.typ}, baseURL: ${Types.zhttpURL.typ}, apiKey: ${Types.secret.typ}, boundary: String) extends ${svc.init} {
            private val authHeaderValue = "Bearer " + apiKey.value.mkString

            $codecs

            ..$implMethods
          }
       """

    ZIO.succeed {
      q"""
        import zio.constraintless.TypeList.::

        trait ${svc.typName} {
          ..$interfaceMethods
        }

        object ${svc.termName} {
          def live: ${Types.zlayer(Types.zhttpClient, ScalaType.nothing, svc).typ} =
            ${Types.zlayer_.term} {
              for {
                client <- ${Types.zio_.term}.service[${Types.zhttpClient.typ}]
                config <- ${Types.zio_.term}.config(${Types.openAIConfig.term}.config).orDie
                boundary <- ${Types.random.term}.nextUUID.map(uuid => "--------" + uuid.toString.replace("-", ""))
              } yield new Live(client, config.baseURL, config.apiKey, boundary)
            }

          ..$accessorMethods

          $liveClass
        }
       """
    }
  }

  private def getParamList(model: Model, endpoint: Endpoint): List[Term.Param] =
    endpoint.parameters.map { param =>
      val paramTyp = param.typ.scalaType(model)
      if (param.isRequired) {
        param"""${param.paramName}: ${paramTyp.typ}"""
      } else {
        param"""${param.paramName}: ${ScalaType.option(paramTyp).typ}"""
      }
    } ++ endpoint.body.toList.map { body =>
      val bodyTyp = body.typ.scalaType(model)
      param"""body: ${bodyTyp.typ}"""
    }

  private def getParamRefs(endpoint: Endpoint): List[Term] =
    endpoint.parameters.map { param =>
      param.paramName
    } ++ endpoint.body.toList.map(_ => bodyParam)

  private val bodyParam: Term.Name = Term.Name("body")
}
