import java.time.LocalDate

import cats.data.NonEmptyList
import shopmatcher.domain.{MatcherError, _}

package object shopmatcher {

  // type aliases for convenience

  type Point = (Double, Double)

  type Errors = List[MatcherError]


  type Wrapped[A] = Either[Errors, A]

  type ListOf[A <: Properties] = NonEmptyList[Feature[A]]

  type WrappedList[A <: Properties] = Wrapped[ListOf[A]]

  type CollectionOf[A <: Properties] = WrappedList[A]

  type WrappedListOf[A] = Wrapped[NonEmptyList[A]]


  type GroupByAgeOf[A] = Map[Option[LocalDate], NonEmptyList[A]]

  type WrappedGroupsByAgeOf[A] = Wrapped[GroupByAgeOf[A]]

}