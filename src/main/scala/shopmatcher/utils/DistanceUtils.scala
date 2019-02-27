package shopmatcher.utils

import cats.data.NonEmptyList
import shopmatcher.{Errors, Point}
import shopmatcher.Rules._
import shopmatcher.domain._
import shopmatcher.utils.ListUtil._

object DistanceUtils {

  def distance(lat1: Double, lon1: Double, lat2: Double, lon2: Double): Double = {
    val earthRadius: Int = 6371000

    val dLat: Double = Math.toRadians(lat2 - lat1)
    val dLng: Double = Math.toRadians(lon2 - lon1)

    val a: Double = Math.sin(dLat / 2) * Math.sin(dLat / 2) +
      Math.cos(Math.toRadians(lat1)) * Math.cos(Math.toRadians(lat2)) * Math.sin(dLng / 2) * Math.sin(dLng / 2)

    val c: Double = 2 * Math.atan2(Math.sqrt(a), Math.sqrt(1 - a))

    earthRadius * c
  }

  // refactor to Boolean function
  def closeness(averageDistance: Double, meters: Int): Distance = if (averageDistance > meters) NotClose else Close

  // count average closeness of a user to a shop
  def averageCloseness(userLocations: NonEmptyList[Feature[User]], shopPoint: Point): Distance = {

    val distances: NonEmptyList[Double] = userLocations.map(userPoint =>
      distance(userPoint.geometry.coordinates._1, userPoint.geometry.coordinates._2, shopPoint._1, shopPoint._2)
    )

    val averageDistance: Double = distances.foldLeft(0D)(_ + _) / distances.length

    closeness(averageDistance, close)
  }

  // count how many times a user was close to a shop
  def timesPassedBy(userLocations: NonEmptyList[Feature[User]], shopPoint: Point): Int = {

    val distances: NonEmptyList[Distance] = userLocations.map(userPoint => {
      closeness(distance(userPoint.geometry.coordinates._1, userPoint.geometry.coordinates._2, shopPoint._1, shopPoint._2), close)
    })


    distances.filterNot(_ == NotClose).length
  }

  def visitedShops(userLocations: NonEmptyList[Feature[User]], shops: NonEmptyList[Feature[Shop]]): Either[Errors, NonEmptyList[Feature[Shop]]] = {

    def checkIfWasInside(user: Point, shopPoint: Point): Boolean =
      distance(user._1, user._2, shopPoint._1, shopPoint._2) <= inside

    def visitedShops: Either[Errors, List[Feature[Shop]]] = {

      // all shops visited by all users of the current group
      val allVisited: NonEmptyList[List[Feature[Shop]]] = shops.map(shop =>
        userLocations.collect { case u if checkIfWasInside(u.geometry.coordinates, shop.geometry.coordinates) => shop }
      )

      // distinct shops for the user group
      val distinctVisited: NonEmptyList[List[Feature[Shop]]] =
        allVisited.map(_.groupBy(_.properties.shopId).map(_._2.head).toList)

      // nonEmptyList of shops
      val verifiedVisited: Either[Errors, List[Feature[Shop]]] =
        flatten(distinctVisited.map(toNonEmptyList(_, EmptyList)))

      verifiedVisited
    }

    for {
      visited <- visitedShops
      nonEmptyVisited <- toNonEmptyList(visited, EmptyList)
    } yield nonEmptyVisited

    // val distinctLocations = allLocations.map(map => map.map(m => m._2.map(u => u.geometry.coordinates)))
  }

}
