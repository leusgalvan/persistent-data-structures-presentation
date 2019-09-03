scalaVersion := "2.12.7"
organization := "com.example"

lazy val `persistent-data-structures` = (project in file("."))
    .settings(
        name := "Persistent Data Structures"
    )

scalacOptions += "-Ypartial-unification"

libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0-RC1"