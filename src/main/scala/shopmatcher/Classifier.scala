package shopmatcher

import java.time.LocalDate
import java.time.chrono.ChronoLocalDate

import cats.data.NonEmptyList
import shopmatcher.domain._
import shopmatcher.utils.CollectionUtil._
import shopmatcher.utils.DistanceCalculator._
import shopmatcher.utils.Parser

object Classifier {

  private def findShopsForGroups(groups: GroupByAgeOf[User]): WrappedGroupsByAgeOf[Feature[Shop]] = {

    val groupedLocations: WrappedGroupsByAgeOf[Feature[User]] = {

      // get all locations of user
      def userPoints(userId: Long): CollectionOf[User] = Matcher.userPoints(userId)

      // get locations of all users
      def locations(users: NonEmptyList[User]): WrappedListOf[Feature[User]] = for {
        userLocations <- flattenList(users.map(user => userPoints(user.userId)))
        nonEmpty <- toNonEmptyList(userLocations, EmptyList)
      } yield nonEmpty

      // get locations grouped
      flattenMap {
        groups.map(m => m._1 -> locations(m._2))
      }
    }

    // find visited shops for each age group
    def findVisitedShops(shops: ListOf[Shop]) =
      groupedLocations
        .map(_
          .map(group =>
            group._1 -> visitedShops(group._2, shops)))

    // find shops that are preferred by each age group
    val shopsForGroups: WrappedGroupsByAgeOf[Feature[Shop]] = for {
      shopList <- Parser.shops // get shops from json
      foundShops <- findVisitedShops(shopList) // find which have been visited
      map <- flattenMap(foundShops)
    } yield map

    shopsForGroups
  }

  // groups of users by age
  private def groupByAge(users: NonEmptyList[User]): WrappedGroupsByAgeOf[User] = {

    implicit val localDateOrdering: Ordering[LocalDate] = Ordering.by(identity[ChronoLocalDate])

    val sortedUsers: List[User] = users.toList.sortBy(_.dateOfBirth)

    // find boundaries of age groups
    val thesholds: Seq[LocalDate] = Range(0, 10).map(i => sortedUsers(i * sortedUsers.length / 10).dateOfBirth)

    // receive user, match it to age category
    def matchToBin(user: User): Option[LocalDate] = thesholds.reverse.find(_.isBefore(user.dateOfBirth))

    // groups of users by age
    val groupsOfUsers = sortedUsers
      .map(user => matchToBin(user) -> user) // sort users by age boundaries
      .groupBy(_._1) // group by boundary
      .map(m => m._1 -> toNonEmptyList(m._2.map(_._2), EmptyList)) // just take users and common boun to nonEmptyList

    flattenMap(groupsOfUsers)
  }

  // find users that are distinct by id
  private def getDistinctUsers: WrappedListOf[User] = for {
    // get users from file, find distinct ones according to id
    users <- Parser.userLocations.map(_.toList.groupBy(_.properties.userId).map(_._2.head.properties).toList)
    nonEmptyUsers <- toNonEmptyList(users, EmptyList)
  } yield nonEmptyUsers

  // find shops that are preferred for each age group
  def classify: WrappedGroupsByAgeOf[Feature[Shop]] = for {
    userList <- getDistinctUsers // find distinct users by id
    groupedUsers <- groupByAge(userList) // split them in categories by age
    res <- findShopsForGroups(groupedUsers) // for each set find where they go
  } yield res

}
