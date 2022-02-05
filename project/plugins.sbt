addSbtPlugin("org.portable-scala" % "sbt-scalajs-crossproject" % "1.1.0")

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "1.8.0")

addSbtPlugin(
  "com.thoughtworks.sbt-scala-js-map" % "sbt-scala-js-map" % "4.1.1"
)

addSbtPlugin(
  "com.thoughtworks.sbt-best-practice" % "sbt-best-practice" % "8.2.1"
)

addSbtPlugin("org.xerial.sbt" % "sbt-sonatype" % "3.9.10")

addSbtPlugin("com.dwijnand" % "sbt-dynver" % "4.1.1")

addSbtPlugin("com.github.sbt" % "sbt-pgp" % "2.1.2")

addSbtPlugin("com.thoughtworks.example" % "sbt-example" % "9.1.0")

libraryDependencies += "net.sourceforge.htmlunit" % "htmlunit" % "2.58.0"

libraryDependencies += "io.circe" %% "circe-generic" % "0.14.1"

libraryDependencies += "com.softwaremill.sttp" %% "circe" % "1.7.2"

libraryDependencies += "org.scalameta" %% "scalameta" % "4.4.34"

addSbtPlugin("ch.epfl.scala" % "sbt-scalajs-bundler" % "0.20.0")
