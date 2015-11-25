enablePlugins(ScalaJSPlugin)

organization := "au.com.realcommercial.binding-scala"

name := "dom"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.3"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.2"

jsDependencies += RuntimeDOM

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
