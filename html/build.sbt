enablePlugins(ScalaJSPlugin)

enablePlugins(Example)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.0.0"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test

libraryDependencies += "net.sourceforge.htmlunit" % "neko-htmlunit" % "2.56.0"

libraryDependencies += "com.thoughtworks.dsl" %%% "reset" % "2.0.0-M2+165-29b36fd5"

libraryDependencies += "com.thoughtworks.dsl" %%% "domains-continuation" % "2.0.0-M2+165-29b36fd5"

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-yield" % "2.0.0-M2+165-29b36fd5"