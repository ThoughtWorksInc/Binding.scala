enablePlugins(ScalaJSPlugin)

libraryDependencies += "com.lihaoyi" %%% "upickle" % "0.4.1"

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

releasePublishArtifactsAction <<= PgpKeys.publishSigned

scalacOptions += "-Xexperimental"
