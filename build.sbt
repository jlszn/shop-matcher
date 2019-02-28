name := "shop-matcher"

version := "0.1"

scalaVersion := "2.12.8"

val circeVersion = "0.11.1"

libraryDependencies ++= Seq(
  "io.circe" %% "circe-core",
  "io.circe" %% "circe-generic",
  "io.circe" %% "circe-parser",
  "io.circe" %% "circe-generic-extras",
).map(_ % circeVersion)

libraryDependencies += "io.circe" %% "circe-derivation" % "0.11.0-M1"