package shopmatcher.utils

import cats.data.NonEmptyList
import shopmatcher.Rules._
import shopmatcher._
import shopmatcher.domain._
import shopmatcher.utils.CollectionUtil._

object DistanceCalculator {

  // Haversine formula for distance
  private def distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val earthRadius: Int = 6371000

    val dLat: Double = Math.toRadians(lat2 - lat1)
    val dLng: Double = Math.toRadians(lon2 - lon1)

    val a: Double = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) * Math.sin(dLng / 2) * Math.sin(dLng / 2)

    val c: Double = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))

    earthRadius * c
  }

  // Close/NotClose case object are more expressive than Boolean value in our case
  private def closeness(averageDistance: Double, meters: Int): Distance = if (averageDistance > meters) NotClose else Close

  // count average closeness of a user to a shop
  def averageCloseness(userLocations: ListOf[User], shopPoint: Point): Distance = {

    val distances: NonEmptyList[Double] = userLocations.map(userPoint =>
      distance(userPoint.geometry.coordinates._1, userPoint.geometry.coordinates._2, shopPoint._1, shopPoint._2)
    )

    val averageDistance: Double = distances.foldLeft(0D)(_ + _) / distances.length

    closeness(averageDistance, close)
  }

  // count how many times a user was close to a shop
  def timesPassedBy(userLocations: ListOf[User], shopPoint: Point): Int = {

    val distances: NonEmptyList[Distance] = userLocations.map(userPoint => {
      closeness(distance(userPoint.geometry.coordinates._1, userPoint.geometry.coordinates._2, shopPoint._1, shopPoint._2), close)
    })


    distances.filterNot(_ == NotClose).length
  }

  // 'passed by' means there exists a UserLocation that is in the area of 'passing by'
  // the area is set in rules and can be changed anytime
  def closeShops(userLocations: ListOf[User]): WrappedListOf[Feature[Shop]] = {

    val shopsAndDistances: Wrapped[List[Feature[Shop]]] =
      Parser.shops.map(_.filterNot(shop => averageCloseness(userLocations, shop.geometry.coordinates) == NotClose))

    val filteredShops: WrappedListOf[Feature[Shop]] = for {
      shops <- shopsAndDistances
      list <- toNonEmptyList(shops, NoCloseShopsFound)
    } yield list

    filteredShops
  }

  // compare all the point of the group members to shop point, to see where they go
  def visitedShops(userLocations: ListOf[User], shops: ListOf[Shop]): WrappedList[Shop] = {

    def checkIfWasInside(user: Point, shopPoint: Point): Boolean = distance(user._1, user._2, shopPoint._1, shopPoint._2) <= inside

    def visitedShops: WrappedList[Shop] = {

      // all shops visited by all users of the current group
      val allVisited: NonEmptyList[List[Feature[Shop]]] = shops.map(shop =>
        userLocations.collect {
          case u if checkIfWasInside(u.geometry.coordinates, shop.geometry.coordinates) => shop
        }
      )

      // distinct shops for the user group
      val distinctVisited: NonEmptyList[List[Feature[Shop]]] =
        allVisited.map(_.groupBy(_.properties.shopId).map(_._2.head).toList)

      // nonEmptyList of shops
      val verifiedVisited: List[Feature[Shop]] = distinctVisited.collect { case g if g.nonEmpty => g }.flatten

      toNonEmptyList(verifiedVisited, EmptyList)
    }

    visitedShops
  }

}
