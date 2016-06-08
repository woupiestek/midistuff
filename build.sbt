name := "midistuff"

version := "1.0"

scalaVersion := "2.11.8"

// the application gets access to midi on a forked jvm and doesn't on sbt's own jvm.
fork in run := true

libraryDependencies += "org.scalaz" % "scalaz-core_2.11" % "7.3.0-M1"
