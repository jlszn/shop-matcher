package shop_matcher.domain

sealed trait MatcherError

case object ParsingError extends MatcherError