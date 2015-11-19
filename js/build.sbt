enablePlugins(ScalaJSPlugin)

enablePlugins(ScalaJSPlay)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.0"

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.3"

jsDependencies += RuntimeDOM

persistLauncher in Compile := true

persistLauncher in Test := false