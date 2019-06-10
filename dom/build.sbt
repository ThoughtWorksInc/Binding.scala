enablePlugins(ScalaJSPlugin)

organization := "com.thoughtworks.binding"

name := "dom"

description := "Reactive web framework for Scala.js."

libraryDependencies += "com.thoughtworks.extractor" %% "extractor" % "1.2.0"

libraryDependencies += "com.lihaoyi" %%% "scalatags" % "0.6.7"

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8" % Test

libraryDependencies += "org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided

libraryDependencies ++= {
  CrossVersion.partialVersion(scalaVersion.value) match {
    case Some((2, scalaMajor)) if scalaMajor >= 11 =>
      Seq("org.scala-lang.modules" %%% "scala-xml" % "1.1.0")
    case _ =>
      Nil
  }
}

scalacOptions ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    Some("-Ymacro-annotations")
  } else {
    None
  }
}

libraryDependencies ++= {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    None
  } else {
    Some(compilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full))
  }
}

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
