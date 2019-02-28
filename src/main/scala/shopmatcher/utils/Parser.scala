package shopmatcher.utils

import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveDecoder
import io.circe.parser._
import shopmatcher.CollectionOf
import shopmatcher.domain.{Properties, _}

import scala.io.Source

object Parser {

  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults
  implicit val userPropertiesDecoder: Decoder[User] = deriveDecoder
  implicit val shopPropertiesDecoder: Decoder[Shop] = deriveDecoder

  private def readGeoJson(path: String): String = Source.fromResource(path).mkString

  private def decodeGeoJson[A <: Properties](data: String)(implicit d: Decoder[A]): CollectionOf[A] =
    decode[FeatureCollection[A]](data).fold(_ => Left(List(ParsingError)), r => Right(r)).map(_.features)

  private def readObjects[A <: Properties](path: String)(implicit d: Decoder[A]): CollectionOf[A] = decodeGeoJson {
    readGeoJson(path)
  }

  val shops: CollectionOf[Shop] = Parser.readObjects[Shop]("testdata/shops.geojson")
  val userLocations: CollectionOf[User] = Parser.readObjects[User]("testdata/users.geojson")

}
