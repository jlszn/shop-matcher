package shop_matcher

import io.circe.Decoder
import io.circe.generic.auto._
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto._
import io.circe.parser._
import shop_matcher.domain._

import scala.io.Source

object Parser/* extends App*/ {

  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults
  implicit val userPropertiesDecoder: Decoder[UserLocation] = deriveDecoder
  implicit val shopPropertiesDecoder: Decoder[Shop] = deriveDecoder

  def decodeGeoJson[A <: Properties](path: String)(implicit d: Decoder[A]): ParsedCollection[A] = {

    val data = Source.fromResource(path).mkString

     decode[FeatureCollection[A]](data).fold(_ => Left(ParsingError), r => Right(r)).map(_.features)
  }

//  println(decodeGeoJson[Shop]("test-data/shops.geojson"))

}
