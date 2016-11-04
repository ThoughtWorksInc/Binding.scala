enablePlugins(ScalaJSPlugin)

organization := "com.thoughtworks.binding"

name := "JsPromiseBinding"

jsDependencies += RuntimeDOM

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.0" % Test

scalacOptions += "-Xexperimental"
