package shopmatcher.utils

import cats.data.NonEmptyList
import shopmatcher.Errors
import shopmatcher.domain.{EmptyList, MatcherError}

object CollectionUtil {

  // transforms List() to NonEmptyList()
  def toNonEmptyList[A](list: List[A], error: MatcherError): Either[Errors, NonEmptyList[A]] =
    NonEmptyList.fromList(list).toRight(List(error))

  // flattens NonEmptyList of Either
  def flatten[A](list: NonEmptyList[Either[Errors, A]]): Either[Errors, List[A]] = {
    val a: Either[List[Errors], List[A]] = list.toList.partition(_.isLeft) match {
      case (Nil, value) => Right(for (Right(i) <- value) yield i)
      case (err, _) => Left(for (Left(s) <- err) yield s)
    }

    a.fold(l => Left(l.flatten), r => Right(r))
  }

  // flattens NonEmptyList of Either
  def flattenList[A](list: NonEmptyList[Either[Errors, NonEmptyList[A]]]): Either[Errors, List[A]] =
    flatten(list).fold(l => Left(l), r => Right(r.flatMap(_.toList)))

  // flattens Map of Either
  def flattenMap[D, T](m: Map[D, Either[Errors, NonEmptyList[T]]]): Either[Errors, Map[D, NonEmptyList[T]]] = {

    val tupled = m.toList

    val t1: List[D] = tupled.map(_._1)
    val t2: List[Either[Errors, NonEmptyList[T]]] = tupled.map(_._2)

    val listOfTuples: List[Either[Errors, (D, NonEmptyList[T])]] = for ((m, a) <- t1 zip t2) yield a.map(m -> _)

    for {
      nonEmpty <- toNonEmptyList(listOfTuples, EmptyList)
      flattened <- flatten(nonEmpty)
    } yield flattened.toMap

  }

}