lazy val BindingT = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build

lazy val BindingSeqT = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build

lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(BindingT, BindingSeqT)

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true

enablePlugins(ScalaUnidocPlugin)

ScalaUnidoc / unidoc / unidocProjectFilter := {
  inAnyProject -- inProjects(Binding.jvm, BindingT.jvm, BindingSeqT.jvm)
}

