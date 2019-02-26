package shopmatcher.domain

sealed trait MatcherError

case object ParsingError extends MatcherError

case object NoUserLocationsFound extends MatcherError

case object NoCloseShopsFound extends MatcherError