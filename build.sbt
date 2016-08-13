lazy val Binding = crossProject.crossType(CrossType.Pure)

lazy val FutureBinding = crossProject.crossType(CrossType.Pure).dependsOn(Binding)

lazy val dom = project.dependsOn(BindingJS)

lazy val JsPromiseBinding = project.dependsOn(BindingJS)

lazy val BindingJS = Binding.js.addSbtFiles(file("../build.sbt.shared"))

lazy val BindingJVM = Binding.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val FutureBindingJS = FutureBinding.js.addSbtFiles(file("../build.sbt.shared"))

lazy val FutureBindingJVM = FutureBinding.jvm.addSbtFiles(file("../build.sbt.shared"))

organization in ThisBuild := "com.thoughtworks.binding"

crossScalaVersions in ThisBuild := Seq("2.10.6", "2.11.8")

developers in ThisBuild := List(
  Developer(
    "Atry",
    "杨博 (Yang Bo)",
    "pop.atry@gmail.com",
    url("https://github.com/Atry")
  )
)

name := "Binding.scala"

publishArtifact := false

lazy val unidoc = project.dependsOn(dom, JsPromiseBinding, BindingJS, FutureBindingJS).settings(scalaJavaUnidocSettings).settings(
  doc in Compile := (UnidocKeys.unidoc in Compile).value.head,
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  scalacOptions += "-Xexperimental",
  scalacOptions in Compile in doc ++= {
    Seq("-doc-title", name.value)
  },
  scalacOptions in Compile in doc ++= {
    Seq("-doc-version", version.value)
  },
  scalacOptions in Compile in doc += "-groups",
  scalacOptions in Compile in doc += "-diagrams",
  scalacOptions in Compile in doc += "-implicits",
  scalacOptions in Compile in doc ++= {
    if (scalaBinaryVersion.value == "2.11") {
      Seq("-author")
    } else {
      Seq()
    }
  },
  publishArtifact in packageSrc := false,
  publishArtifact in packageBin := false
)

licenses in ThisBuild += "MIT" -> url("http://opensource.org/licenses/MIT")

startYear in ThisBuild := Some(2016)

homepage in ThisBuild := Some(url(s"https://github.com/ThoughtWorksInc/${name.value}"))

scmInfo in ThisBuild := Some(ScmInfo(
  url(s"https://github.com/ThoughtWorksInc/${name.value}"),
  s"scm:git:git://github.com/ThoughtWorksInc/${name.value}.git",
  Some(s"scm:git:git@github.com:ThoughtWorksInc/${name.value}.git")))

releaseUseGlobalVersion := true

releaseCrossBuild := true

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

releasePublishArtifactsAction <<= PgpKeys.publishSigned
