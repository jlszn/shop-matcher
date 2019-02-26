package shopmatcher.utils

import cats.data.NonEmptyList
import shopmatcher.Point
import shopmatcher.Rules.close
import shopmatcher.domain._

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

  def closeness(averageDistance: Double): Distance = if (averageDistance > close) NotClose else Close

  // count average closeness of a user to a shop
  def averageCloseness(userLocations: NonEmptyList[Feature[UserLocation]], shopPoint: Point): Distance = {

    val distances: NonEmptyList[Double] = userLocations.map(userPoint =>
      distance(userPoint.geometry.coordinates._1, userPoint.geometry.coordinates._2, shopPoint._1, shopPoint._2)
    )

    val averageDistance: Double = distances.foldLeft(0D)(_ + _) / distances.length

    closeness(averageDistance)
  }

  // count how many times a user was close to a shop
  def timesPassedBy(userLocations: NonEmptyList[Feature[UserLocation]], shopPoint: Point): Int = {

    val distances: NonEmptyList[Distance] = userLocations.map(userPoint => closeness {
      distance(userPoint.geometry.coordinates._1, userPoint.geometry.coordinates._2, shopPoint._1, shopPoint._2)
    })

    distances.filterNot(_ == NotClose).length
  }

}
