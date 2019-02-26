import cats.data.NonEmptyList
import shopmatcher.domain._

package object shopmatcher {

  type CollectionOf[A <: Properties] = Either[MatcherError, NonEmptyList[Feature[A]]]

  type Point = (Double, Double)

  type WrappedShops = Either[MatcherError, NonEmptyList[Feature[Shop]]]

  type Shops = NonEmptyList[Feature[Shop]]

  type UserLocations = NonEmptyList[Feature[UserLocation]]

  def toNonEmptyList[A](list: List[A], error: MatcherError): Either[MatcherError, NonEmptyList[A]] = NonEmptyList.fromList(list).toRight(error)

}
