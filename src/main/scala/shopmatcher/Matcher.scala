package shopmatcher

import cats.data.NonEmptyList
import shopmatcher.domain._
import shopmatcher.utils.CollectionUtil._
import shopmatcher.utils.DistanceCalculator._
import shopmatcher.utils.Parser

object Matcher {

  // shops that are frequently being passed by
  private def frequentShops(userLocations: ListOf[User], shops: ListOf[Shop], top: Int): WrappedListOf[Feature[Shop]] = {

    // go over user locations and see how many times user gets close to each of the close shops
    val shopsWithFrequency: NonEmptyList[(Feature[Shop], Int)] =
      shops.map(shop => (shop, timesPassedBy(userLocations, shop.geometry.coordinates)))

    // find the most frequently passed by shops
    val topShopsByFrequency: List[Feature[Shop]] = shopsWithFrequency.toList.sortBy(_._2).map(_._1).take(top)

    toNonEmptyList(topShopsByFrequency, NoCloseShopsFound)
  }

  // get user's points by id
  def userPoints(userId: Long): CollectionOf[User] = for {
    userList <- Parser.userLocations.map(_.filter(_.properties.userId == userId))
    nonEmptyLocations <- toNonEmptyList(userList, NoUserLocationsByIdFound)
  } yield nonEmptyLocations

  // find most applicable shops
  def mostApplicableShops(userId: Long, top: Int): WrappedListOf[Feature[Shop]] = for {
    locations <- userPoints(userId)
    shops <- closeShops(locations)
    resultingShops <- frequentShops(locations, shops, top)
  } yield resultingShops

}