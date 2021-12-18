lazy val BindingT = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

lazy val BindingSeqT = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(BindingT)

lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(BindingT, BindingSeqT)

lazy val BindableSeq = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(Binding)

lazy val NodeBinding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).dependsOn(BindableSeq)

lazy val html = project.dependsOn(NodeBinding.js)

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true
