import sbt._

object Dependencies {
  lazy val scalaTest = "org.scalatest" %% "scalatest" % "3.0.3"
  lazy val cats = "org.typelevel" %% "cats-core" % "1.1.0"
  lazy val catsEffect = "org.typelevel" %% "cats-effect" % "0.10"
  lazy val catsMTL = "org.typelevel" %% "cats-mtl-core" % "0.2.1"
  lazy val scalaSTM = "org.scala-stm" %% "scala-stm" % "0.8"
}
