val scalaJSVersion = Option(System.getenv("SCALAJS_VERSION")).getOrElse("0.6.21")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % scalaJSVersion)

addSbtPlugin("com.thoughtworks.sbt-scala-js-map" % "sbt-scala-js-map" % "2.0.0")

addSbtPlugin("com.thoughtworks.sbt-best-practice" % "sbt-best-practice" % "latest.release")
