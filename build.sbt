lazy val Binding = crossProject.crossType(CrossType.Pure)

lazy val FutureBinding = crossProject.crossType(CrossType.Pure).dependsOn(Binding)

lazy val dom = project.dependsOn(BindingJS)

lazy val JsPromiseBinding = project.dependsOn(BindingJS)

lazy val BindingJS = Binding.js.addSbtFiles(file("../build.sbt.shared"))

lazy val BindingJVM = Binding.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val FutureBindingJS = FutureBinding.js.addSbtFiles(file("../build.sbt.shared"))

lazy val FutureBindingJVM = FutureBinding.jvm.addSbtFiles(file("../build.sbt.shared"))

organization in ThisBuild := "com.thoughtworks.binding"

crossScalaVersions := Seq(
  "2.10.6",
  "2.11.8",
  "2.12.0"
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

lazy val unidoc = project
  .enablePlugins(StandaloneUnidoc)
  .settings(
    UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := {
      inAnyProject -- inProjects(BindingJVM, FutureBindingJVM)
    },
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    scalacOptions += "-Xexperimental"
  )

startYear in ThisBuild := Some(2015)

scalaJSUseRhino in ThisBuild := false
