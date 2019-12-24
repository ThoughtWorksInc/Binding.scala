parallelExecution in Global := false

lazy val SafeBuffer = crossProject.crossType(CrossType.Pure)

lazy val Binding = crossProject.crossType(CrossType.Pure).dependsOn(SafeBuffer)

lazy val SafeBufferJS = SafeBuffer.js

lazy val SafeBufferJVM = SafeBuffer.jvm

lazy val BindingJS = Binding.js

lazy val BindingJVM = Binding.jvm

organization in ThisBuild := "com.thoughtworks.binding"

publish / skip := true

enablePlugins(ScalaUnidocPlugin)

ScalaUnidoc / unidoc / unidocProjectFilter := {
  inAnyProject -- inProjects(SafeBufferJVM, BindingJVM)
}

scalacOptions += "-Ymacro-annotations"

scalacOptions += "-Xexperimental"
