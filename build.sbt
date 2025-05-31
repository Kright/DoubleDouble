ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "3.7.0"

lazy val root = (project in file("."))
  .settings(
    name := "DoubleDouble",
    idePackagePrefix := Some("com.github.kright"),
    libraryDependencies ++= Seq(
      "org.scalatest" %% "scalatest" % "3.2.19" % "test",
      "org.scalatestplus" %% "scalacheck-1-18" % "3.2.19.0" % "test"
    )
  )
