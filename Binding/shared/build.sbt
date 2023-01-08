enablePlugins(Example)

description := "Reactive data-binding for Scala. This artifact is available for both Scala.js and JVM."

libraryDependencies ++= {
  if (scalaBinaryVersion.value == "2.13") {
    Some("com.thoughtworks.sde" %%% "core" % "3.3.4")
  } else {
    None
  }
}

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.14" % Test

libraryDependencies ++= {
  if (scalaBinaryVersion.value == "2.13") {
    Some("org.scala-lang" % "scala-reflect" % scalaVersion.value)
  } else {
    None
  }
}

libraryDependencies ++= {
  if (scalaBinaryVersion.value == "2.13") {
    Some("org.scala-lang" % "scala-compiler" % scalaVersion.value % Provided)
  } else {
    None
  }
}

scalacOptions += "-Ymacro-annotations"
