enablePlugins(ScalaJSPlugin)

libraryDependencies += {
  import Ordering.Implicits._
  if (VersionNumber(scalaVersion.value).numbers >= Seq(2L, 13L)) {
    "com.lihaoyi" %%% "upickle" % "0.7.5"
  } else {
    "com.lihaoyi" %%% "upickle" % "0.4.4"
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

libraryDependencies += "org.scala-js" %%% "scalajs-dom" % "0.9.7"

scalacOptions += "-Xexperimental"

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.0.8" % Test

jsDependencies += RuntimeDOM % Test
