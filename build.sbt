lazy val rx = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).build

lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).dependsOn(rx)

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true

enablePlugins(ScalaUnidocPlugin)

ScalaUnidoc / unidoc / unidocProjectFilter := {
  inAnyProject -- inProjects(rx.jvm, Binding.jvm)
}

