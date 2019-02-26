package shopmatcher

import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveDecoder
import shopmatcher.domain.{Feature, Shop, UserLocation}
import shopmatcher.utils.Parser

// Also if possible  create a classifier according to the age.
object Classifier {

  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults
  implicit val userPropertiesDecoder: Decoder[UserLocation] = deriveDecoder
  implicit val shopPropertiesDecoder: Decoder[Shop] = deriveDecoder

  val shops: CollectionOf[Shop] = Parser.readObjects[Shop]("testdata/shops.geojson")
  val userLocations: CollectionOf[UserLocation] = Parser.readObjects[UserLocation]("testdata/users.geojson")


  // find places that each person has been in list[(user, list[shops])]
  // брать спискок, сортировать по возрасту, делить на 10 частей и смотреть куда они ходят
  // дальше в рамках бина просто сортировать по популярности

  def visitedShops(user: UserLocation) = ???

  def usersToLocations: List[(UserLocation, List[Feature[Shop]])] = ???

}
