lazy val root = (project in file(".")).
  settings(
    name := "tautology-verifier",
    organization := "com.sakthipriyan",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.8"
  )

libraryDependencies ++= Seq(
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)
