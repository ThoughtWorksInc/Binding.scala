enablePlugins(ScalaJSPlugin)

organization := "com.thoughtworks.binding"

name := "JsPromiseBinding"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % Test

scalacOptions += "-Xexperimental"
