enablePlugins(PlayScala)

enablePlugins(PlayScalaJS)

name := """functional-data-binding-demo"""

version := "1.0-SNAPSHOT"

lazy val root = project in file(".") dependsOn coreJvm

lazy val js = project dependsOn coreJs

lazy val core = crossProject.crossType(CrossType.Pure)

lazy val coreJvm = core.jvm.addSbtFiles(file("../build.sbt"))

lazy val coreJs = core.js.addSbtFiles(file("../build.sbt"))

scalaJSProjects := Seq(js, coreJs)

scalaVersion in ThisBuild := "2.11.7"

libraryDependencies ++= Seq(
  jdbc,
  cache,
  ws,
  specs2 % Test
)

resolvers += "scalaz-bintray" at "http://dl.bintray.com/scalaz/releases"

// Play provides two styles of routers, one expects its actions to be injected, the
// other, legacy style, accesses its actions statically.
routesGenerator := InjectedRoutesGenerator


fork in run := true