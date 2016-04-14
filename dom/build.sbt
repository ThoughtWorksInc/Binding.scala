enablePlugins(ScalaJsMap)

enablePlugins(ScalaJSPlugin)

organization := "com.thoughtworks.binding"

name := "dom"

description := "Reactive web framework for Scala.js."

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.5.3"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.8.2"

jsDependencies += RuntimeDOM

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")

resolvers in ThisBuild += Resolver.sonatypeRepo("releases")

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

releasePublishArtifactsAction <<= PgpKeys.publishSigned
