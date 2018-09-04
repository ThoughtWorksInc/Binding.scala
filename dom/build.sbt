enablePlugins(ScalaJSPlugin)

organization := "com.thoughtworks.binding"

name := "dom"

description := "Reactive web framework for Scala.js."

libraryDependencies += "com.thoughtworks.extractor" %% "extractor" % "1.1.1"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.2"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.1"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.4" % Test

libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)

scalacOptions += "-Xexperimental"

jsDependencies in Test += RuntimeDOM

inConfig(Test) {
  jsEnv in ThisBuild := RhinoJSEnv().value
}
