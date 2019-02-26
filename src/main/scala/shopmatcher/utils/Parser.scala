package shopmatcher.utils

import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._
import io.circe.parser._
import shopmatcher.CollectionOf
import shopmatcher.domain.{Properties, _}

import scala.io.Source

object Parser /* extends App*/ {

  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults
  implicit val userPropertiesDecoder: Decoder[UserLocation] = deriveDecoder
  implicit val shopPropertiesDecoder: Decoder[Shop] = deriveDecoder

  def readGeoJson(path: String): String = Source.fromResource(path).mkString

  def decodeGeoJson[A <: Properties](data: String)(implicit d: Decoder[A]): CollectionOf[A] =
    decode[FeatureCollection[A]](data).fold(_ => Left(ParsingError), r => Right(r)).map(_.features)

  def readObjects[A <: Properties](path: String)(implicit d: Decoder[A]): CollectionOf[A] = decodeGeoJson {
    readGeoJson(path)
  }

  def encodeGeoJson[A <: Properties](data: CollectionOf[A]): String = ???

  def writeGeoJson[A <: Properties](data: String): String = ???

  def writeObjects[A <: Properties](data: CollectionOf[A]): String = writeGeoJson {
    encodeGeoJson(data)
  }

  //  println(decodeGeoJson[Shop]("test-data/shops.geojson"))

}
