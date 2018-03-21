import Dependencies._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization := "com.example",
      scalaVersion := "2.12.3",
      version      := "0.1.0-SNAPSHOT"
    )),
    name := "Hello",

    scalacOptions += "-Ypartial-unification",

    libraryDependencies += scalaTest % Test,
    libraryDependencies += cats,
    libraryDependencies += catsEffect,
    libraryDependencies += scalaSTM,
  )
