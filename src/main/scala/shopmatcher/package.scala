import cats.data.NonEmptyList
import shopmatcher.domain.{MatcherError, _}

package object shopmatcher {

  type CollectionOf[A <: Properties] = Either[Errors, NonEmptyList[Feature[A]]]

  type Point = (Double, Double)


  type WrappedShops = Either[Errors, NonEmptyList[Feature[Shop]]]

  type Shops = NonEmptyList[Feature[Shop]]

  type Users = NonEmptyList[Feature[User]]

  type Errors = List[MatcherError]

}
