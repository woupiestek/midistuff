import scalariform.formatter.preferences._

name := "midistuff"

version := "1.0"

scalaVersion := "2.12.4"

// the application gets access to midi on a forked jvm and doesn't on sbt's own jvm.
fork in run := true
outputStrategy := Some(StdoutOutput)
connectInput in run := true

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.2.20",
  "org.scalatest" %% "scalatest" % "3.0.0" % "test")

scalacOptions ++= Seq("-feature", "-deprecation")

scalafixSettings
//sbtfixSettings
