package shopmatcher

import java.time.LocalDate

import cats.data.NonEmptyList
import io.circe.Decoder
import io.circe.generic.extras.Configuration
import io.circe.generic.extras.semiauto.deriveDecoder
import shopmatcher.domain._
import shopmatcher.utils.DistanceUtils._
import shopmatcher.utils.ListUtil._
import shopmatcher.utils.Parser

object Classifier {

  implicit val config: Configuration = Configuration.default.withSnakeCaseMemberNames.withDefaults
  implicit val userPropertiesDecoder: Decoder[User] = deriveDecoder
  implicit val shopPropertiesDecoder: Decoder[Shop] = deriveDecoder

  val shops: CollectionOf[Shop] = Parser.readObjects[Shop]("testdata/shops.geojson")
  val userLocations: CollectionOf[User] = Parser.readObjects[User]("testdata/users.geojson")

  // get user's points by id
  def userLocations(userId: Long): CollectionOf[User] = for {
    userList <- userLocations.map(l => l.filter(_.properties.userId == userId))
    nonEmptyLocations <- toNonEmptyList(userList, NoUserLocationsFound)
  } yield nonEmptyLocations

  def findShopsForGroups(groups: Map[Option[LocalDate], NonEmptyList[User]]): Either[Errors, Map[Option[LocalDate], NonEmptyList[Feature[Shop]]]] = {
    // find locations by user id -> flatten -> to Set by coordinates

    val allLocations: Either[Errors, Map[Option[LocalDate], NonEmptyList[Feature[User]]]] = {

      def locations(m: NonEmptyList[User]): Either[Errors, NonEmptyList[Feature[User]]] = for {
        userLocations <- flatten(m.map(user => userLocations(user.userId)))
        nonEmpty <- toNonEmptyList(userLocations, EmptyList)
      } yield nonEmpty

      flattenMap {
        groups.map(m => m._1 -> locations(m._2))
      }
    }

    val shopsForGroups: Either[Errors, Map[Option[LocalDate], NonEmptyList[Feature[Shop]]]] = for {
      shopList <- shops
      foundShops <- allLocations.map(_.map(group => group._1 -> visitedShops(group._2, shopList)))
      f <- flattenMap(foundShops)
    } yield f

    shopsForGroups
  }

  def groupByAge(users: NonEmptyList[User]): Either[MatchError, Map[Option[LocalDate], NonEmptyList[User]]] = {

    implicit val localDateOrdering: Ordering[LocalDate] = Ordering.by(_.toEpochDay)

    val sortedUsers: NonEmptyList[User] = users.sortBy(_.dateOfBirth)

    // boundaries of age groups
    val thesholds: Seq[LocalDate] = Range(0, 10).map(i => sortedUsers.toList(i * sortedUsers.length / 10).dateOfBirth)

    def matchToBin(user: User): Option[LocalDate] = thesholds.reverse.find(_.isBefore(user.dateOfBirth))

    // Some(date) represents the lowest boundary of a group, List[User] - users that correspond to it
    // where date is None - the group is the oldest, has no lowest boundary
    val groupedByAge: Map[Option[LocalDate], NonEmptyList[User]] = sortedUsers.map(user => matchToBin(user) -> user)
      .groupBy(_._1) // group by boundary
      .map(m => m._1 -> m._2.map(_._2)) // just take the common boundary and users

    Either(groupedByAge)
  }

  def getDistinctUsers: Either[Errors, NonEmptyList[User]] = for {
    a <- userLocations.map(_.groupBy(_.properties.userId).map(_._2.head.properties).toList)
    res <- toNonEmptyList(a, NoUserLocationsFound)
  } yield res

  def usersToLocations: NonEmptyList[(User, List[Feature[Shop]])] = {
    // find distinct users, sort them by age - List[User] +
    // split them in 10 categories by age - Map[Option[LocalDate], List[User]] +

    // for each set find where they go in distinctevely - Map[Option[LocalDate], List[Shop]]]

    // for each list compare their locations with shops and find where they were inside, filter out the same shops ->

    // List[List[(User->List[Feature[Shop]])]]

    // get all users with distinct ids

    for {
      // list of users with distinct ids sorted by birthdate
      userList <- getDistinctUsers
      groupedUsers <- groupByAge(userList)
      res <- findShopsForGroups(groupedUsers)
    } yield res
  }

}
