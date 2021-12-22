lazy val StreamTPolyfill = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

lazy val `domains-CovariantStreamT` = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(StreamTPolyfill)

lazy val `domains-PatchStreamT` = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(`domains-CovariantStreamT`)

lazy val DefaultFuture = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

lazy val `keywords-Bind` = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(`domains-CovariantStreamT`)

lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(`domains-CovariantStreamT`, `domains-PatchStreamT`, DefaultFuture)

lazy val `bindable-BindableSeq` = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(Binding)

lazy val NodeBinding = project.dependsOn(`bindable-BindableSeq`.js)

lazy val htmldefinitions = project

lazy val html = project.dependsOn(htmldefinitions, NodeBinding)

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true
