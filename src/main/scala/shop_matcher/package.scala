import cats.data.NonEmptyList
import shop_matcher.domain.{Feature, MatcherError, Properties}

package object shop_matcher {

  type ParsedCollection[A <: Properties] = Either[MatcherError, NonEmptyList[Feature[A]]]

}
