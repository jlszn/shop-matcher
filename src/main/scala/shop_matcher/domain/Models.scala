package shop_matcher.domain

import java.time.Instant

import cats.data.NonEmptyList

sealed trait Properties

case class Shop(gmlId: String,
                shopId: Long,
                shopName: String,
                inseeComm: Int,
                porX: Int,
                porY: Int) extends Properties

case class UserLocation(userId: Long,
                        userName: String,
                        timestamp: Instant,
                        dateOfBirth: Instant,
                        inseeComm: Int,
                        porX: Int,
                        porY: Int) extends Properties

case class Geometry(`type`: String, coordinates: (Double, Double))

case class Feature[A <: Properties](`type`: String, geometry: Geometry, properties: A)

case class FeatureCollection[A <: Properties](`type`: String, features: NonEmptyList[Feature[A]])
