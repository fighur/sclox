val scala3Version = "3.2.1"

lazy val root = project
  .in(file("."))
  .settings(
    name := "sclox",
    version := "0",

    scalaVersion := scala3Version
  )
