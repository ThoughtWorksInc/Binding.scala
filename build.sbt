lazy val core = crossProject.crossType(CrossType.Pure)

lazy val dom = project dependsOn coreJS

lazy val coreJS = core.js.addSbtFiles(file("../build.sbt.shared"))

lazy val coreJVM = core.jvm.addSbtFiles(file("../build.sbt.shared"))

scalaVersion in ThisBuild := "2.11.7"

developers in ThisBuild := List(
  Developer(
    "Atry",
    "杨博 (Yang Bo)",
    "pop.atry@gmail.com",
    url("https://github.com/Atry")
  )
)

val projectName = "Binding.scala"

publishArtifact := false

licenses in ThisBuild += "MIT" -> url("http://opensource.org/licenses/MIT")

startYear in ThisBuild := Some(2016)

homepage in ThisBuild := Some(url(s"https://github.com/ThoughtWorksInc/$projectName"))

scmInfo in ThisBuild := Some(ScmInfo(
  url(s"https://github.com/ThoughtWorksInc/$projectName"),
  s"scm:git:git://github.com/ThoughtWorksInc/$projectName.git",
  Some(s"scm:git:git@github.com:ThoughtWorksInc/$projectName.git")))

releaseUseGlobalVersion := true

releaseCrossBuild := false

import ReleaseTransformations._

releaseProcess := Seq[ReleaseStep](
  checkSnapshotDependencies,
  inquireVersions,
  runClean,
  runTest,
  setReleaseVersion,
  commitReleaseVersion,
  tagRelease,
  publishArtifacts,
  setNextVersion,
  commitNextVersion,
  releaseStepCommand("sonatypeRelease"),
  pushChanges
)

sonatypeProfileName := "com.thoughtworks.binding"