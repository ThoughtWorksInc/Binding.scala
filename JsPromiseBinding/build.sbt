enablePlugins(ScalaJSPlugin)

organization := "com.thoughtworks.binding"

name := "JsPromiseBinding"

jsDependencies += RuntimeDOM

libraryDependencies += "org.scalatest" %% "scalatest" % "3.0.0" % Test

resolvers in ThisBuild += Resolver.sonatypeRepo("releases")

releasePublishArtifactsAction <<= PgpKeys.publishSigned

scalacOptions += "-Xexperimental"
