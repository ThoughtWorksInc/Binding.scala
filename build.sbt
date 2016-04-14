lazy val core = crossProject.crossType(CrossType.Pure)

lazy val dom = project dependsOn coreJS

lazy val coreJS = core.js.addSbtFiles(file("../build.sbt.shared"))

lazy val coreJVM = core.jvm.addSbtFiles(file("../build.sbt.shared"))

organization := "com.thoughtworks.binding"

name := "unidoc"

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

publishArtifact in packageSrc := false

publishArtifact in packageBin := false

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

sonatypeProfileName := "com.thoughtworks.binding"

pgpSecretRing := baseDirectory.value / "secret" / "secring.asc"

pgpPublicRing := baseDirectory.value / "pubring.asc"

pgpPassphrase := Some(Array.empty)

releaseProcess := {
  releaseProcess.value.patch(releaseProcess.value.indexOf(pushChanges), Seq[ReleaseStep](releaseStepCommand("sonatypeRelease")), 0)
}

releaseProcess -= runClean

releaseProcess -= runTest

scalaJavaUnidocSettings

import UnidocKeys._
unidocProjectFilter in ScalaUnidoc in unidoc := inAnyProject -- inProjects(coreJS)

doc in Compile := (unidoc in Compile).value.head

releasePublishArtifactsAction <<= PgpKeys.publishSigned

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full)
