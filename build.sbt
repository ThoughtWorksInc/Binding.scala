lazy val StreamTPolyfill = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

lazy val CovariantStreamT = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(StreamTPolyfill)

lazy val PatchStreamT = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(CovariantStreamT)

lazy val DefaultFuture = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(CovariantStreamT, PatchStreamT, DefaultFuture)

lazy val BindableSeq = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(Binding)

lazy val NodeBinding = project.dependsOn(BindableSeq.js)

lazy val htmldefinitions = project

lazy val html = project.dependsOn(htmldefinitions, NodeBinding)

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true
