name := "midistuff"

version := "1.0"

scalaVersion := "2.11.8"

// the application gets access to midi on a forked jvm and doesn't on sbt's own jvm.
fork in run := true
outputStrategy := Some(StdoutOutput)
connectInput in run := true

libraryDependencies ++= Seq(
  "org.scalaz" %% "scalaz-core" % "7.3.0-M6",
  "org.scalatest" % "scalatest_2.11" % "3.0.0"
)

scalacOptions ++= Seq("-feature", "-deprecation")
