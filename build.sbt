ThisBuild / version := "0.1.0-SNAPSHOT"

ThisBuild / scalaVersion := "2.13.8"

lazy val root = (project in file("."))
  .settings(
    name := "ztm-dsa-scala"
  )

libraryDependencies += "com.lihaoyi" %% "pprint" % "0.7.3"
