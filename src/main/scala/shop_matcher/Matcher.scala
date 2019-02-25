package shop_matcher

import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveDecoder
import shop_matcher.domain.{FeatureCollection, MatcherError, Shop, UserLocation}

// We would like to match the users with the most applicable shops based on the distance as well as on the frequency they pass by.
// Also if possible  create a classifier according to the age.


object Matcher {

  // dtos ?

  // how do I match one to another

  // calculate medium passing-by distance and match it to case objects

  // make case classes representing passing-by frequency ()
  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults
  implicit val userPropertiesDecoder: Decoder[UserLocation] = deriveDecoder
  implicit val shopPropertiesDecoder: Decoder[Shop] = deriveDecoder

  val shops: ParsedCollection[Shop] = Parser.decodeGeoJson[Shop]("test-data/shops.geojson")
  val userLocations: ParsedCollection[UserLocation] = Parser.decodeGeoJson[UserLocation]("test-data/users.geojson")


}