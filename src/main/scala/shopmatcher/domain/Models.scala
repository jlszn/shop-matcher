package shopmatcher.domain

import java.time.{Instant, LocalDate}

import cats.data.NonEmptyList
import shopmatcher.Point

sealed trait Properties

case class Shop(gmlId: String,
                shopId: Long,
                shopName: String,
                inseeComm: Int,
                porX: Int,
                porY: Int) extends Properties

case class User(userId: Long,
                userName: String,
                timestamp: LocalDate,
                dateOfBirth: LocalDate) extends Properties

case class Geometry(`type`: String, coordinates: Point)

case class Feature[A <: Properties](`type`: String, geometry: Geometry, properties: A)

case class FeatureCollection[A <: Properties](`type`: String, features: NonEmptyList[Feature[A]])


sealed trait Distance // distance of passing by

case object Close extends Distance

case object NotClose extends Distance

case object Inside extends Distance
