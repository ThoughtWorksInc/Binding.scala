lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

lazy val html = project.dependsOn(Binding.js)

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true
