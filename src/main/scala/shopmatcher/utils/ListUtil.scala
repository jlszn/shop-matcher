package shopmatcher.utils

import cats.data.{EitherT, NonEmptyList}
import com.sun.xml.internal.ws.policy.privateutil.PolicyUtils.IO
import shopmatcher.Errors
import shopmatcher.domain.{EmptyList, MatcherError}

import scala.concurrent.Future

object ListUtil {


  def a(a: Either[String, Long]): String = ???

  def a (a: Either[String, Int]): String = {
    a(a.map(_.toLong))
  }

  def toNonEmptyList[A](list: List[A], error: MatcherError): Either[Errors, NonEmptyList[A]] =
    NonEmptyList.fromList(list).toRight(List(error))

  def flatten[A](list: NonEmptyList[Either[Errors, A]]): Either[Errors, List[A]] = {
    val a: Either[List[Errors], List[A]] = list.toList.partition(_.isLeft) match {
      case (Nil, value) => Right(for (Right(i) <- value) yield i)
      case (err, _) => Left(for (Left(s) <- err) yield s)
    }

    a.fold(l => Left(l.flatten), r => Right(r))
  }

  def flattenList[A](list: NonEmptyList[Either[Errors, NonEmptyList[A]]]): Either[Errors, List[A]] =
    flatten(list).fold(l => Left(l), r => Right(r.flatten))

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
        flattenedList <- flattenList(list)
        nonEmpty <- toNonEmptyList(flattenedList, EmptyList)
      } yield tupled.map(_._1).map(_ -> nonEmpty)

    listOfTuples.map(_.toMap)
  }

  //type ServiceResult[A] = EitherT[Future, Errors, A]
  //def flatten1[B](list: List[ServiceResult[B]]): ServiceResult[List[B]] = ???
  /*
    def flattenMap[A, T](m: Map[ServiceResult[A], ServiceResult[T]]): ServiceResult[Map[A, T]] = {

      val tupled = m.toList

      val listOfTuples: ServiceResult[List[(A, T)]] =
        for {
          keyList <- flatten1(tupled.map(_._1))
          valueList <- flatten1(tupled.map(_._2))
        } yield for {
          key <- keyList
          value <- valueList
        } yield key -> value

      listOfTuples.map(_.toMap)
    }
  */

  def flattenMap[D, T](m: Map[Either[Errors, D], NonEmptyList[T]]): Either[Errors, Map[D, NonEmptyList[T]]] = {

    val tupled = m.toList

    val l: List[Either[Errors, D]] = tupled.map(_._1)
    val r: List[NonEmptyList[T]] = tupled.map(_._2)

    val listOfTuples =
      for {
        listL <- toNonEmptyList(l, EmptyList)
        fl <- flatten(listL)
        listR <- toNonEmptyList(r, EmptyList)
      } yield fl -> listR

    val k = listOfTuples.map(_.toMap)
    k
  }

}