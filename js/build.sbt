enablePlugins(ScalaJSPlugin)

enablePlugins(ScalaJSPlay)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")
