lazy val BindingT = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

lazy val BindingSeqT = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(BindingT)

lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(BindingT, BindingSeqT)

lazy val BindableSeq = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(Binding)

lazy val NodeBinding = project.dependsOn(BindableSeq.js)

lazy val htmldefinitions = project

lazy val html = project.dependsOn(htmldefinitions, NodeBinding)

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true
