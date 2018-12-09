enablePlugins(ScalaJSPlugin)

libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.4"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.6"

scalacOptions += "-Xexperimental"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.5" % Test

jsDependencies += RuntimeDOM % Test
