parallelExecution in Global := false

lazy val Binding = crossProject.crossType(CrossType.Pure)

lazy val FutureBinding = crossProject.crossType(CrossType.Pure).dependsOn(Binding)

lazy val dom = project.dependsOn(BindingJS).dependsOn(XmlExtractorJS)

lazy val Route = project.dependsOn(BindingJS)

lazy val JsPromiseBinding = project.dependsOn(BindingJS)

lazy val BindingJS = Binding.js.addSbtFiles(file("../build.sbt.shared"))

lazy val BindingJVM = Binding.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val FutureBindingJS = FutureBinding.js.addSbtFiles(file("../build.sbt.shared"))

lazy val FutureBindingJVM = FutureBinding.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val XmlExtractor = crossProject.crossType(CrossType.Pure)

lazy val XmlExtractorJS = XmlExtractor.js.addSbtFiles(file("../build.sbt.shared"))

lazy val XmlExtractorJVM = XmlExtractor.jvm.addSbtFiles(file("../build.sbt.shared"))

lazy val fxml = crossProject.crossType(CrossType.Pure).dependsOn(Binding, XmlExtractor)

lazy val fxmlJS = fxml.js.addSbtFiles(file("../build.sbt.shared"))

lazy val fxmlJVM = fxml.jvm.addSbtFiles(file("../build.sbt.shared"))

organization in ThisBuild := "com.thoughtworks.binding"

crossScalaVersions := Seq(
  "2.10.7",
  "2.11.12",
  "2.12.8"
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
  .enablePlugins(TravisUnidocTitle)
  .settings(
    UnidocKeys.unidocProjectFilter in ScalaUnidoc in UnidocKeys.unidoc := {
      inAnyProject -- inProjects(XmlExtractorJVM, BindingJVM, FutureBindingJVM, fxmlJS)
    },
    addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.0" cross CrossVersion.full),
    scalacOptions += "-Xexperimental"
  )

startYear in ThisBuild := Some(2015)
