lazy val StreamT =
  crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

lazy val CovariantStreamT = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .dependsOn(StreamT)

lazy val PatchStreamT = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .dependsOn(CovariantStreamT)

lazy val DefaultFuture =
  crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Full).build()

lazy val Observable = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .dependsOn(CovariantStreamT, PatchStreamT, DefaultFuture)

lazy val Binding = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .dependsOn(Observable)

lazy val LegacyBinding = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .dependsOn(CovariantStreamT, PatchStreamT, DefaultFuture)

lazy val `keywords-Bind` = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .dependsOn(CovariantStreamT)

lazy val `bindable-BindableSeq` = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .dependsOn(LegacyBinding)

lazy val `bindable-Bindable` = crossProject(JSPlatform, JVMPlatform)
  .crossType(CrossType.Full)
  .dependsOn(LegacyBinding)

lazy val `html-Definitions` = project

lazy val `html-InterpolationParser` = project

lazy val html =
  project.dependsOn(
    `html-Definitions`,
    `html-InterpolationParser`,
    `bindable-BindableSeq`.js,
    `bindable-Bindable`.js,
    `keywords-Bind`.js % Test
  )

ThisBuild / organization := "com.thoughtworks.binding"

publish / skip := true

enablePlugins(ScalaUnidocPlugin)
