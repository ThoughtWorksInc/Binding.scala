lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true
