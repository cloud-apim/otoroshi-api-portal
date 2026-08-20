import Dependencies._

ThisBuild / scalaVersion     := "3.8.4"
ThisBuild / version          := "1.0.0-dev"
ThisBuild / organization     := "com.cloud-apim"
ThisBuild / organizationName := "Cloud-APIM"

lazy val root = (project in file("."))
  .settings(
    name := "otoroshi-api-portal",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked"),
    resolvers += "jitpack" at "https://jitpack.io",
    libraryDependencies ++= Seq(
      "fr.maif" %% "otoroshi" % "18.0.0-preview2" % "provided",
      munit % Test
    )
  )
