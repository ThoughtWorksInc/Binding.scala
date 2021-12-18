enablePlugins(ScalaJSPlugin)

enablePlugins(Example)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.0.0"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test

libraryDependencies += "net.sourceforge.htmlunit" % "neko-htmlunit" % "2.56.0"
