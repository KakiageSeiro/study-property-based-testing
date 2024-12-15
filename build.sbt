ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.6.2"

lazy val root = (project in file("."))
  .settings(
    name := "study-property-based-testing"
  )

libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.19" % Test

// プロパティベースドテストのライブラリ
libraryDependencies += "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % "test"
