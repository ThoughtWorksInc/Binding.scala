parallelExecution in Global := false

lazy val Binding = crossProject.crossType(CrossType.Pure)

lazy val FutureBinding = crossProject.crossType(CrossType.Pure).dependsOn(Binding)

lazy val dom = project.dependsOn(BindingJS).dependsOn(XmlExtractorJS)

lazy val JsPromiseBinding = project.dependsOn(BindingJS)

lazy val BindingJS = Binding.js.addSbtFiles(file("../build.sbt.shared"))

lazy val BindingJVM = Binding.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val FutureBindingJS = FutureBinding.js.addSbtFiles(file("../build.sbt.shared"))

lazy val FutureBindingJVM = FutureBinding.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val XmlExtractor = crossProject.crossType(CrossType.Pure)

lazy val XmlExtractorJS = XmlExtractor.js.addSbtFiles(file("../build.sbt.shared"))

lazy val XmlExtractorJVM = XmlExtractor.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val fxml = project.dependsOn(BindingJVM, XmlExtractorJVM)

organization in ThisBuild := "com.thoughtworks.binding"

crossScalaVersions := Seq(
  "2.10.6",
  "2.11.8"
  // Don't build on 2.12.0-RC1 because ScalaTags did not support 2.12.0-RC1 yet
  // "2.12.0-RC1"
)

developers in ThisBuild := List(
  Developer(
    "Atry",
    "杨博 (Yang Bo)",
    "pop.atry@gmail.com",
    url("https://github.com/Atry")
  )
)

name in ThisBuild := "Binding.scala"

publishArtifact := false

lazy val unidoc = project.settings(scalaJavaUnidocSettings).settings(
  UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := inAnyProject -- inProjects(XmlExtractorJVM, BindingJVM, FutureBindingJVM),
  doc in Compile := (UnidocKeys.unidoc in Compile).value.head,
  addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
  scalacOptions += "-Xexperimental",
  scalacOptions in Compile in doc ++= {
    Seq("-doc-title", (name in ThisBuild).value)
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
  publishArtifact in packageBin := false,
  releasePublishArtifactsAction <<= PgpKeys.publishSigned
)

licenses in ThisBuild += "MIT" -> url("http://opensource.org/licenses/MIT")

startYear in ThisBuild := Some(2015)

homepage in ThisBuild := Some(url(s"https://github.com/ThoughtWorksInc/${(name in ThisBuild).value}"))

scmInfo in ThisBuild := Some(ScmInfo(
  url(s"https://github.com/ThoughtWorksInc/${(name in ThisBuild).value}"),
  s"scm:git:git://github.com/ThoughtWorksInc/${(name in ThisBuild).value}.git",
  Some(s"scm:git:git@github.com:ThoughtWorksInc/${(name in ThisBuild).value}.git")))

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
