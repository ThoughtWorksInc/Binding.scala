lazy val cross = crossProject.crossType(CrossType.Pure) in file(".")

lazy val js = cross.js.addSbtFiles(file("../build.sbt.shared"))

lazy val jvm = cross.jvm.addSbtFiles(file("../build.sbt.shared"))

scalaVersion in ThisBuild := "2.11.7"