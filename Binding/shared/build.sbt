enablePlugins(Example)

description := "Reactive data-binding for Scala. This artifact is available for both Scala.js and JVM."

libraryDependencies += "com.thoughtworks.sde" %%% "core" % "3.3.4"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.13" % Test

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

scalacOptions += "-Ymacro-annotations"
