addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.7")

addSbtPlugin("com.thoughtworks.sbt-api-mappings" % "sbt-api-mappings" % "0.2.1")

addSbtPlugin("com.jsuereth" % "sbt-pgp" % "1.0.0")

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "0.5.0")

lazy val root = project in file(".") dependsOn `sbt-release`

// Use a SNAPSHOT version of sbt-release due to a bug in sbt-release 1.0.1(https://github.com/sbt/sbt-release/pull/122)
lazy val `sbt-release` = RootProject(uri("https://github.com/sbt/sbt-release.git#master"))
