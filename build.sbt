import Dependencies._

ThisBuild / scalaVersion     := "3.0.0-RC2"
ThisBuild / version          := "0.1.0-SNAPSHOT"
ThisBuild / organization     := "com.example"
ThisBuild / organizationName := "example"

lazy val root = (project in file("."))
  .settings(
    name := "Project Euler Speedrun"
  )
