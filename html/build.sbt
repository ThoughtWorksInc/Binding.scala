enablePlugins(ScalaJSPlugin)

enablePlugins(ScalaJSBundlerPlugin)

enablePlugins(Example)

Test / requireJsDomEnv := true

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "2.0.0"

libraryDependencies += "net.sourceforge.htmlunit" % "neko-htmlunit" % "2.56.0"

libraryDependencies += "com.thoughtworks.dsl" %%% "reset" % "2.0.0-M2+228-ad681156"

libraryDependencies += "com.thoughtworks.dsl" %%% "domains-continuation" % "2.0.0-M2+228-ad681156"

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-yield" % "2.0.0-M2+228-ad681156"

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-await" % "2.0.0-M2+228-ad681156" % Test
