package shopmatcher.utils

import cats.data.NonEmptyList
import shopmatcher.Errors
import shopmatcher.domain.{EmptyList, MatcherError}

object ListUtil {

  def toNonEmptyList[A](list: List[A], error: MatcherError): Either[Errors, NonEmptyList[A]] =
    NonEmptyList.fromList(list).toRight(List(error))

  def flatten[A](list: NonEmptyList[Either[Errors, NonEmptyList[A]]]): Either[Errors, List[A]] = {
    list.toList.partition(_.isLeft) match {
      case (Nil, value) => Right(for (Right(i) <- value) yield i)
      case (err, _) => Left(for (Left(s) <- err) yield s)
    }
  }.fold(l => Left(l.flatten), r => Right(r.flatten))

  /*def flattenMap[D, T](m: Map[D, Either[Errors, List[T]]]): Either[Errors, Map[D, List[T]]] = {

    val tupled = m.toList

    val t2: List[Either[Errors, NonEmptyList[T]]] = tupled.map(_._2).map(t => for {
      tt <- t
      res <- toNonEmptyList(tt, EmptyList)
    } yield res)

    val et2: Either[Errors, NonEmptyList[Either[Errors, NonEmptyList[T]]]] = toNonEmptyList(t2, EmptyList)

    val listOfTuples: Either[Errors, List[(D, List[T])]] =
      for {
        e <- et2
        valueList <- flatten(e)
      } yield for {
        key <- tupled.map(_._1)
        //value <- valueList
      } yield key -> valueList

    listOfTuples.map(_.toMap)
  }*/

  def flattenMap[D, T](m: Map[D, Either[Errors, NonEmptyList[T]]]): Either[Errors, Map[D, NonEmptyList[T]]] = {

    val tupled = m.toList

    val listOfTuples: Either[Errors, List[(D, NonEmptyList[T])]] =
      for {
        list <- toNonEmptyList(tupled.map(_._2), EmptyList)
        flattenedList <- flatten(list)
        nonEmpty <- toNonEmptyList(flattenedList, EmptyList)
      } yield tupled.map(_._1).map(_ -> nonEmpty)

    listOfTuples.map(_.toMap)
  }

}