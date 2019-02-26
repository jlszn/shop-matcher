package shopmatcher

import cats.data.NonEmptyList
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveDecoder
import shopmatcher.domain._
import shopmatcher.utils.DistanceUtils._
import shopmatcher.utils.Parser

object Matcher {

  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults
  implicit val userPropertiesDecoder: Decoder[UserLocation] = deriveDecoder
  implicit val shopPropertiesDecoder: Decoder[Shop] = deriveDecoder

  val shops: CollectionOf[Shop] = Parser.readObjects[Shop]("testdata/shops.geojson")
  val userLocations: CollectionOf[UserLocation] = Parser.readObjects[UserLocation]("testdata/users.geojson")

  // 'passed by' means there exists a UserLocation that is in the area of 'passing by'
  // the area is set in rules and can be changed anytime
  def closeShops(userLocations: UserLocations): WrappedShops = {

    val shopsAndDistances: Either[MatcherError, NonEmptyList[(Feature[Shop], Distance)]] =
      shops.map(_.map(shop => shop -> averageCloseness(userLocations, shop.geometry.coordinates)))

    val filteredShops: WrappedShops = for {
      shops <- shopsAndDistances
      list <- toNonEmptyList(shops.filterNot(m => m._2 == NotClose), NoCloseShopsFound)
    } yield list.map(_._1)

    filteredShops
  }

  // shops the are frequently being passed by
  def frequentShops(userLocations: UserLocations, shops: Shops, top: Int): WrappedShops = {

    // go over user locations and see how many times user gets close to each of the close shops
    val shopsWithFrequency: NonEmptyList[(Feature[Shop], Int)] =
      shops.map(shop => (shop, timesPassedBy(userLocations, shop.geometry.coordinates)))

    // find the most frequently passed by shops
    val topShopsByFrequency: List[Feature[Shop]] = shopsWithFrequency.sortBy(_._2).map(_._1).toList.take(top)

    toNonEmptyList(topShopsByFrequency, NoCloseShopsFound)
  }

  // get user's points by id
  def userLocations(userId: Long): CollectionOf[UserLocation] = for {
    userList <- userLocations.map(l => l.filter(_.properties.userId == userId))
    nonEmptyLocations <- toNonEmptyList(userList, NoUserLocationsFound)
  } yield nonEmptyLocations

  // find most applicable shops
  def mostApplicableShops(userId: Long, top: Int): Either[MatcherError, Shops] = for {
    locations <- userLocations(userId)
    shops <- closeShops(locations)
    resultingShops <- frequentShops(locations, shops, top)
  } yield resultingShops

}