package shopmatcher

import cats.data.NonEmptyList
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveDecoder
import shopmatcher.domain._
import shopmatcher.utils.DistanceUtils._
import shopmatcher.utils.Parser

// Also if possible  create a classifier according to the age.

// find places that each person has been in list[(user, list[shops])]
// list.groupBy(_._1.age)

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

  def mostApplicableCloseShops(userLocations: UserLocations, shops: Shops, top: Int): WrappedShops = {

    // go over user locations and see how many times user gets close to each of the close shops
    val shopsWithFrequency: NonEmptyList[(Feature[Shop], Int)] =
      shops.map(shop => (shop, timesPassedBy(userLocations, shop.geometry.coordinates)))

    // find the most frequently passed by shops
    val topShopsByFrequency: List[Feature[Shop]] = shopsWithFrequency.sortBy(_._2).map(_._1).toList.take(top)

    toNonEmptyList(topShopsByFrequency, NoCloseShopsFound)
  }

  def usersLocations(userId: Long): CollectionOf[UserLocation] = for {
    userList <- userLocations.map(l => l.filter(_.properties.userId == userId))
    nonEmptyLocations <- toNonEmptyList(userList, NoUserLocationsFound)
  } yield nonEmptyLocations

  def getShops(userId: Long, top: Int): Either[MatcherError, Shops] = for {
    locations <- usersLocations(userId)
    shops <- closeShops(locations)
    resultingShops <- mostApplicableCloseShops(locations, shops, top)
  } yield resultingShops

}