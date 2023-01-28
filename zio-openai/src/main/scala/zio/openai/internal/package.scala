package zio.openai

import zio.json.ast.Json
import zio.prelude._
import zio.schema.{ DynamicValue, Schema, StandardType, TypeId }
import zio.{ Chunk, NonEmptyChunk }

import java.util.Base64
import scala.collection.immutable.ListMap

package object internal {

  implicit def nonEmptyChunkSchema[A: Schema]: Schema[NonEmptyChunk[A]] =
    Schema
      .chunk[A]
      .transformOrFail(
        (chunk: Chunk[A]) =>
          chunk
            .nonEmptyOrElse[Either[String, NonEmptyChunk[A]]](Left("Must be non-empty"))(Right(_)),
        (nonEmptyChunk: NonEmptyChunk[A]) => Right(nonEmptyChunk.toChunk)
      )

  // TODO: zio-schema needs to directly encode Dynamic into JSON
  implicit lazy val jsonObjectSchema: Schema[Map[String, Json]] =
    Schema.dynamicValue.transformOrFail[Map[String, Json]](
      {
        case DynamicValue.Record(_, record) =>
          record.toList
            .foldLeftM(Map.empty[String, Json]) { case (acc, (k, v)) =>
              dynamicValueToJson(v).map((json: Json) => acc + (k -> json))
            }
        case _                              =>
          Left("Must be a record")
      },
      fields =>
        Right(
          DynamicValue.Record(
            TypeId.Structural,
            ListMap.from(fields.map { case (k, v) => k -> jsonToDynamicValue(v) })
          )
        )
    )

  private def jsonToDynamicValue(json: Json): DynamicValue =
    json match {
      case Json.Obj(fields)   =>
        DynamicValue.Record(
          TypeId.Structural,
          ListMap.from(fields.map { case (k, v) => k -> jsonToDynamicValue(v) })
        )
      case Json.Arr(elements) => DynamicValue.Sequence(elements.map(jsonToDynamicValue))
      case Json.Bool(value)   => DynamicValue.Primitive(value, StandardType.BoolType)
      case Json.Str(value)    => DynamicValue.Primitive(value, StandardType.StringType)
      case Json.Num(value)    => DynamicValue.Primitive(value, StandardType.BigDecimalType)
      case Json.Null          => DynamicValue.NoneValue
    }

  private def dynamicValueToJson(dyn: DynamicValue): Either[String, Json] =
    dyn match {
      case DynamicValue.Record(id, values) =>
        values.toList
          .foldLeftM(Map.empty[String, Json]) { case (acc, (k, v)) =>
            dynamicValueToJson(v).map((json: Json) => acc + (k -> json))
          }
          .map(map => Json.Obj(Chunk.fromIterable(map)))

      case DynamicValue.Enumeration(id, value)         =>
        Left("Enumeration is not supported")
      case DynamicValue.Sequence(values)               =>
        values
          .foldLeftM(Chunk.empty[Json]) { case (acc, v) =>
            dynamicValueToJson(v).map(acc :+ _)
          }
          .map(Json.Arr(_))
      case DynamicValue.Dictionary(entries)            =>
        Left("Dictionary is not supported")
      case DynamicValue.SetValue(values)               =>
        Left("Set is not supported")
      case DynamicValue.Primitive(value, standardType) =>
        Right(standardType.asInstanceOf[StandardType[_]] match {
          case StandardType.UnitType   => Json.Obj()
          case StandardType.StringType => Json.Str(value.asInstanceOf[String])
          case StandardType.BoolType   => Json.Bool(value.asInstanceOf[Boolean])
          case StandardType.ByteType   => Json.Num(value.asInstanceOf[Byte])
          case StandardType.ShortType  => Json.Num(value.asInstanceOf[Short])
          case StandardType.IntType    => Json.Num(value.asInstanceOf[Int])
          case StandardType.LongType   => Json.Num(value.asInstanceOf[Long])
          case StandardType.FloatType  => Json.Num(value.asInstanceOf[Float])
          case StandardType.DoubleType => Json.Num(value.asInstanceOf[Double])
          case StandardType.BinaryType =>
            Json.Str(Base64.getEncoder.encodeToString(value.asInstanceOf[Chunk[Byte]].toArray))
          case _                       => Json.Str(value.toString)
        })
      case DynamicValue.Singleton(instance)            =>
        Right(Json.Obj())
      case DynamicValue.SomeValue(value)               =>
        dynamicValueToJson(value).map(Json.Arr(_))
      case DynamicValue.NoneValue                      =>
        Right(Json.Null)
      case DynamicValue.Tuple(left, right)             =>
        Left("Tuple is not supported")
      case DynamicValue.LeftValue(value)               =>
        Left("Either is not supported")
      case DynamicValue.RightValue(value)              =>
        Left("Either is not supported")
      case DynamicValue.DynamicAst(ast)                =>
        Left("DynamicAst is not supported")
      case DynamicValue.Error(message)                 =>
        Left(message)
    }
}
