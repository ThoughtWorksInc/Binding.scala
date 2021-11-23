lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true

Global / scalaVersion := "3.1.0"
