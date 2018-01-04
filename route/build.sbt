enablePlugins(ScalaJSPlugin)

libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.4"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.4"

scalacOptions += "-Xexperimental"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % Test

jsEnv := new org.scalajs.jsenv.jsdomnodejs.JSDOMNodeJSEnv
