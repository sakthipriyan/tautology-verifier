lazy val root = (project in file(".")).
  settings(
    name := "tautology-verifier",
    organization := "com.sakthipriyan",
    version := "1.0-SNAPSHOT",
    scalaVersion := "2.11.8"
  )

libraryDependencies ++= Seq(
  "ch.qos.logback" % "logback-classic" % "1.1.7",
  "org.scalatest" %% "scalatest" % "2.2.6" % "test"
)
