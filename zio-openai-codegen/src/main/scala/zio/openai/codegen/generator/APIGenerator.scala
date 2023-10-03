package zio.openai.codegen.generator

import io.github.vigoo.metagen.core.{ CodeFileGenerator, Generator, GeneratorFailure, ScalaType }
import zio.ZIO
import zio.nio.file.Path
import zio.openai.codegen.model.{
  API,
  ContentType,
  Endpoint,
  Model,
  RequestBody,
  ResponseBody,
  TypeDefinition
}

import scala.meta.*

trait APIGenerator {
  this: HasParameters & ModelGenerator =>
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

  private def generateScaladoc(endpoint: Endpoint): ZIO[CodeFileGenerator, Nothing, Mod.Annot] =
    CodeFileGenerator.addScaladoc(
      buildScaladoc(
        endpoint.summary.getOrElse(""),
        None,
        endpoint.parameters.map(param => param.name -> param.description)
      )
    )

  private def generateFlatScaladoc(
    endpoint: Endpoint,
    obj: TypeDefinition.Object
  ): ZIO[CodeFileGenerator, Nothing, Mod.Annot] =
    CodeFileGenerator.addScaladoc(
      buildScaladoc(
        endpoint.summary.getOrElse(""),
        None,
        obj.fields.map(field => field.scalaName -> field.description)
      )
    )

  // TODO: clean up (extract parts)
  private def generateApiClass(
    model: Model,
    api: API
  ): ZIO[CodeFileGenerator, OpenAIGeneratorFailure, Term.Block] = {
    println(s"Generating API class for '${api.name}'")
    val svc = ScalaType(Packages.openai, api.name)

    val isSvcDeprecated = api.endpoints.forall(_.isDeprecated)

    for {
      interfaceMethods <-
        ZIO
          .foreach(api.endpoints) { endpoint =>
            val paramList = getParamList(model, endpoint)
            val responseType = endpoint.responseType(model)
            val base: Decl.Def =
              q"def ${endpoint.methodName}(..$paramList): ${Types.zio(ScalaType.any, Types.openAIFailure, responseType).typ}"

            endpoint.hasSingleBodyParameter(model) match {
              case Some(obj) =>
                val cons = endpoint.body.get.typ.scalaType(model).term
                val fieldList = getObjectFieldsAsParams(
                  model,
                  obj.fields,
                  allowDefaults = true,
                  filterStreamingControl = true
                )
                val fieldNames = getObjectFieldNames(obj, replaceStreamingWith = Some(false))
                val flat: Defn.Def =
                  q"""def ${endpoint.methodName}(..$fieldList): ${Types
                      .zio(ScalaType.any, Types.openAIFailure, responseType)
                      .typ} =
                  ${endpoint.methodName}($cons(..$fieldNames))
             """

                for {
                  doc                     <- generateScaladoc(endpoint)
                  flatDoc                 <- generateFlatScaladoc(endpoint, obj)
                  optionalStreamingMethods =
                    if (endpoint.hasStreamingOverride(model)) {
                      val streaming =
                        q"""def ${endpoint.methodNameStreaming}(..$fieldList): ${Types
                            .zstream(ScalaType.any, Types.openAIFailure, responseType)
                            .typ}"""
                      List(
                        if (endpoint.isDeprecated)
                          streaming
                            .copy(mods = doc :: Mod.Annot(init"deprecated()") :: streaming.mods)
                        else
                          streaming
                      )
                    } else {
                      List.empty
                    }
                } yield List[Stat](
                  if (endpoint.isDeprecated)
                    base.copy(mods = doc :: Mod.Annot(init"deprecated()") :: base.mods)
                  else base.copy(mods = doc :: base.mods),
                  if (endpoint.isDeprecated)
                    flat.copy(mods = flatDoc :: Mod.Annot(init"deprecated()") :: flat.mods)
                  else flat.copy(mods = flatDoc :: flat.mods)
                ) ++ optionalStreamingMethods
              case None      =>
                for {
                  doc <- generateScaladoc(endpoint)
                } yield List(
                  if (endpoint.isDeprecated)
                    base.copy(mods = doc :: Mod.Annot(init"deprecated()") :: base.mods)
                  else base.copy(mods = doc :: base.mods)
                )
            }
          }
          .map(_.flatten)
      accessorMethods  <-
        ZIO
          .foreach(api.endpoints) { endpoint =>
            val paramList = getParamList(model, endpoint)
            val paramRefs = getParamRefs(endpoint)
            val responseType = endpoint.responseType(model)

            val base =
              q"""def ${endpoint.methodName}(..$paramList): ${Types
                  .zio(svc, Types.openAIFailure, responseType)
                  .typ} =
              ${Types.zio_.term}.serviceWithZIO(_.${endpoint.methodName}(..$paramRefs))
         """

            endpoint.hasSingleBodyParameter(model) match {
              case Some(obj) =>
                val cons = endpoint.body.get.typ.scalaType(model).term
                val fieldList = getObjectFieldsAsParams(
                  model,
                  obj.fields,
                  allowDefaults = true,
                  filterStreamingControl = true
                )
                val fieldNames = getObjectFieldNames(obj, replaceStreamingWith = Some(false))
                val flat: Defn.Def =
                  q"""def ${endpoint.methodName}(..$fieldList): ${Types
                      .zio(svc, Types.openAIFailure, responseType)
                      .typ} =
                    ${endpoint.methodName}($cons(..$fieldNames))
               """

                for {
                  doc                     <- generateScaladoc(endpoint)
                  flatDoc                 <- generateFlatScaladoc(endpoint, obj)
                  optionalStreamingMethods =
                    if (endpoint.hasStreamingOverride(model)) {
                      val fieldNames = fieldList.map(p => Term.Name(p.name.value))
                      val streaming =
                        q"""def ${endpoint.methodNameStreaming}(..$fieldList): ${Types
                            .zstream(svc, Types.openAIFailure, responseType)
                            .typ} =
                              ${Types.zstream_.term}.serviceWithStream(_.${endpoint.methodNameStreaming}(..$fieldNames))
                         """
                      List(
                        if (endpoint.isDeprecated)
                          streaming
                            .copy(mods = doc :: Mod.Annot(init"deprecated()") :: streaming.mods)
                        else
                          streaming
                      )
                    } else {
                      List.empty
                    }
                } yield List(
                  if (endpoint.isDeprecated)
                    base.copy(mods = doc :: Mod.Annot(init"deprecated()") :: base.mods)
                  else base.copy(mods = doc :: base.mods),
                  if (endpoint.isDeprecated)
                    flat.copy(mods = flatDoc :: Mod.Annot(init"deprecated()") :: flat.mods)
                  else flat.copy(mods = flatDoc :: flat.mods)
                ) ++ optionalStreamingMethods

              case None =>
                for {
                  doc <- CodeFileGenerator.addScaladoc(endpoint.summary.getOrElse(""))
                } yield List(
                  if (endpoint.isDeprecated)
                    base.copy(mods = doc :: Mod.Annot(init"deprecated()") :: base.mods)
                  else base.copy(mods = doc :: base.mods)
                )
            }
          }
          .map(_.flatten)

      implMethods <-
        ZIO.foreach(api.endpoints) { endpoint =>
          val paramList = getParamList(model, endpoint)
          val responseType = endpoint.responseType(model)

          val queryParameters = endpoint.queryParameters
          val pathParameters = endpoint.pathParameters

          val basePathString = Lit.String(endpoint.pathPattern)
          val pathString =
            if (pathParameters.isEmpty)
              basePathString
            else
              pathParameters.foldLeft[Term](basePathString) { case (str, param) =>
                val litParamName = Lit.String("{" + param.name + "}")
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
              q"""${Types.zhttpQueryParams.term}(List(..$optionalPairs).map(_.toOption).flatten.map { case (k, v) => (k, ${Types.chunk_.term}(v)) } : _*)"""
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
              q"""baseURL.withPath(baseURL.path ++ $path).withQueryParams($queryParams)"""
            else
              q"""baseURL.withPath(baseURL.path ++ $path)"""

          val body =
            endpoint.body match {
              case Some(RequestBody(ContentType.`application/json`, typ))    =>
                val bodyType = typ.scalaType(model)
                q"""${Types.zio_.term}.succeed(${Types.encoders.term}.toJsonBody[${bodyType.typ}](this.codecs, $bodyParam))"""
              case Some(RequestBody(ContentType.`multipart/form-data`, typ)) =>
                val bodyType = typ.scalaType(model)
                q"""${Types.zio_.term}.fromEither(${Types.encoders.term}.toMultipartFormDataBody[${bodyType.typ}]($bodyParam, this.boundary))
                        .mapError(${Types.openAIFailure.term}.EncodingError(_))
                 """
              case None                                                      =>
                q"""${Types.zio_.term}.succeed(${Types.zhttpBody.term}.empty)"""
            }

          val contentType =
            if (endpoint.body.exists(_.contentType == ContentType.`multipart/form-data`))
              q"""${Types.zhttpHeader.term}.ContentType(${endpoint.bodyContentTypeAsMediaType}, boundary = Some(this.boundary))"""
            else
              q"""${Types.zhttpHeader.term}.ContentType(${endpoint.bodyContentTypeAsMediaType})"""

          val request =
            q"""${Types.zhttpRequest.term}.default(
               method = ${endpoint.method.constructor.term},
               url = $url,
               body = body
             ).addHeader(authHeader)
              .addHeader($contentType)
             """

          val mapResponse =
            endpoint.response match {
              case Some(responseBody)
                  if responseBody.contentType == ContentType.`application/json` =>
                val responseType = responseBody.typ.scalaType(model)
                q"""${Types.decoders.term}.tryDecodeJsonResponse[${responseType.typ}](this.codecs, req, response)"""
              case None =>
                q"""${Types.decoders.term}.validateEmptyResponse(req, response)"""
              case _    =>
                throw new IllegalArgumentException(
                  s"Unsupported response content type: ${endpoint.response}"
                )
            }

          val base =
            q"""def ${endpoint.methodName}(..$paramList): ${Types
                .zio(ScalaType.any, Types.openAIFailure, responseType)
                .typ} = {
                  $body.flatMap { body =>
                    val req = $request
                    client.request(req).mapError(${Types.openAIFailure.term}.Unknown(_)).flatMap { response =>
                      $mapResponse
                    }
                  }
                }
            """

          for {
            doc   <- generateScaladoc(endpoint)
            result = if (endpoint.isDeprecated)
                       base.copy(mods = doc :: Mod.Annot(init"deprecated()") :: base.mods)
                     else base.copy(mods = doc :: base.mods)
          } yield result
        }

      jsonTypes = api.endpoints.flatMap { endpoint =>
                    endpoint.body.toList.collect {
                      case RequestBody(ContentType.`application/json`, typ) =>
                        typ
                    } ++
                      endpoint.response.toList.collect {
                        case ResponseBody(ContentType.`application/json`, typ) =>
                          typ
                      }
                  }

      typeList = jsonTypes.foldRight[Type](Types.typeListEnd.typ) { (typ, lst) =>
                   val scalaType = typ.scalaType(model)
                   t"""${scalaType.typ} :: $lst"""
                 }
      codecs   =
        q"""private val codecs: ${Types.binaryCodecs.typ}[$typeList] = {
              import _root_.zio.schema.codec.JsonCodec.schemaBasedBinaryCodec
              ${Types.binaryCodecs.term}.make
            }
          """

      liveClass =
        q"""class Live(client: ${Types.zhttpClient.typ}, baseURL: ${Types.zhttpURL.typ}, apiKey: ${Types.secret.typ}, boundary: ${Types.zhttpBoundary.typ}) extends ${svc.init} {
              private val authHeader = ${Types.zhttpHeader.term}.Authorization.Bearer(apiKey.value.mkString)

              $codecs

              ..$implMethods
            }
         """

      ifaceBase =
        q"""
           trait ${svc.typName} {
             ..$interfaceMethods
           }
         """
      iface     =
        if (isSvcDeprecated) ifaceBase.copy(mods = Mod.Annot(init"deprecated()") :: ifaceBase.mods)
        else ifaceBase

      liveBase =
        q"""
           def live: ${Types.zlayer(Types.zhttpClient, ScalaType.nothing, svc).typ} =
            ${Types.zlayer_.term} {
              for {
                client <- ${Types.zio_.term}.service[${Types.zhttpClient.typ}]
                config <- ${Types.zio_.term}.config(${Types.openAIConfig.term}.config).orDie
                boundary <- ${Types.zhttpBoundary.term}.randomUUID
              } yield new Live(client, config.baseURL, config.apiKey, boundary)
            }
         """
      live     =
        if (isSvcDeprecated) liveBase.copy(mods = Mod.Annot(init"deprecated()") :: liveBase.mods)
        else liveBase

      defaultBase =
        q"""def default: ${Types.zlayer(ScalaType.any, Types.throwable, svc).typ} =
              ${Types.zhttpClient.term}.default >>> live
         """
      default     =
        if (isSvcDeprecated)
          defaultBase.copy(mods = Mod.Annot(init"deprecated()") :: defaultBase.mods)
        else defaultBase

    } yield Term.Block(
      List(
        q"""import zio.constraintless.TypeList.::""",
        iface,
        q"""
           object ${svc.termName} {
             $live
             $default
  
             ..$accessorMethods
  
             $liveClass
           }
         """
      )
    )
  }

  private def getParamList(model: Model, endpoint: Endpoint): List[Term.Param] =
    endpoint.parameters.map { param =>
      val paramTyp = param.typ.scalaType(model)
      if (param.isRequired) {
        param"""${param.paramName}: ${paramTyp.typ}"""
      } else {
        param"""${param.paramName}: ${Types.optional(paramTyp).typ}"""
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
