enablePlugins(ScalaJSPlugin)

organization := "com.thoughtworks.binding"

name := "dom"

description := "Reactive web framework for Scala.js."

libraryDependencies += "com.thoughtworks.extractor" %% "extractor" % "1.2.1"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8" % Test

libraryDependencies += "org.typelevel" %% "macro-compat" % "1.1.1"

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %%% "scala-xml" % "1.1.0")
    case _ =>
      Nil
  }
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

scalacOptions += "-Xexperimental"

scalacOptions ++= {
  if (scalaBinaryVersion.value == "2.10") {
    Nil
  } else {
    Seq("-Ywarn-unused-import")
  }
}

jsDependencies in Test += RuntimeDOM

inConfig(Test) {
  jsEnv in ThisBuild := RhinoJSEnv().value
}
