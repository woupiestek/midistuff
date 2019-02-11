logLevel := Level.Warn
resolvers += Resolver.typesafeRepo("releases")

addSbtPlugin("org.scalariform" % "sbt-scalariform" % "1.8.2")
addSbtPlugin("ch.epfl.scala" % "sbt-scalafix" % "0.9.4")
