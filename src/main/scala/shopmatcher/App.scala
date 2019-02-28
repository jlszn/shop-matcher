package shopmatcher

import shopmatcher.domain.{NoCloseShopsFound, NoUserLocationsByIdFound, ParsingError}

object App {

  private def error(e: Errors): Unit = e.foreach {
    case ParsingError => println("\nBad JSON file\n")
    case NoCloseShopsFound => println("\nNo applicable shops found\n")
    case NoUserLocationsByIdFound => println("\nNo locations for user with such id found\n")
    case _ => println("\nError occurred\n")
  }

  private def printClassify(): Unit = {
    val res = Classifier.classify

    res match {
      case Right(r) => r.foreach(group => {

        println("\nAge boundary: " + group._1.getOrElse("none, oldest group"))
        println("Preferred shops:")

        group._2.map(l => println(l.properties.shopName))

      })
      case Left(e) => error(e)
    }
    println()
  }

  private def printBestShops(userId: Long, top: Int): Unit = {
    val res = Matcher.mostApplicableShops(userId, top)

    res match {
      case Right(r) =>

        println("\nMost applicable shops:\n")
        r.map(shop => println(shop.properties.shopName))
        println()

      case Left(e) => error(e)
    }
  }

  def main(args: Array[String]): Unit = {

    args.length match {
      case 1 => args(0) match {
        case "classify" => printClassify()
      }
      case 3 => (args(0), args(1), args(2)) match {
        case ("match", id, top) => printBestShops(id.toLong, top.toInt)
        case _ => System.exit(0)
      }
      case _ =>
        println("\nBad input")
        System.exit(0)
    }

  }

}
