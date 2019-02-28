package shopmatcher.utils

import java.io.File

import com.typesafe.config.{Config, ConfigFactory}

object PathConfig {

  private val config: Config = ConfigFactory.parseFile(new File("./src/main/resources/application.conf"))

  val pathToShops: String = config.getString("path.shops")
  val pathToUsers: String = config.getString("path.users")

}