name := "midistuff"

version := "1.0"

scalaVersion := "2.11.8"

// the application gets access to midi on a forked jvm and doesn't on sbt's own jvm.
fork in run := true

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.3.0-M1",
  "com.typesafe.akka" %% "akka-actor" % "2.4.7"
)

scalacOptions ++= Seq("-feature", "-deprecation")
