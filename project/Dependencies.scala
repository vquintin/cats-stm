import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.3"
  lazy val cats = "org.typelevel" %% "cats-core" % "1.1.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "0.10"
}
