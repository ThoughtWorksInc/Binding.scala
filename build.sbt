import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

parallelExecution in Global := false

lazy val SafeBuffer = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).build

lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).dependsOn(SafeBuffer)

lazy val FutureBinding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).dependsOn(Binding)

lazy val dom = project.dependsOn(Binding.js).dependsOn(XmlExtractor.js)

lazy val Route = project.dependsOn(Binding.js)

lazy val JsPromiseBinding = project.dependsOn(Binding.js)

lazy val XmlExtractor = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).build

lazy val fxml = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).dependsOn(Binding, XmlExtractor)

organization in ThisBuild := "com.thoughtworks.binding"

publish / skip := true

enablePlugins(ScalaUnidocPlugin)

ScalaUnidoc / unidoc / unidocProjectFilter := {
  inAnyProject -- inProjects(SafeBuffer.jvm, XmlExtractor.jvm, Binding.jvm, FutureBinding.jvm, fxml.js)
}

addCompilerPlugin("org.scalamacros" % "paradise" % "2.1.1" cross CrossVersion.full)

scalacOptions += "-Xexperimental"
