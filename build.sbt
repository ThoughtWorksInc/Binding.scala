import sbtcrossproject.CrossPlugin.autoImport.{crossProject, CrossType}

parallelExecution in Global := false

lazy val SafeBuffer = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).build

lazy val Binding = crossProject(JSPlatform, JVMPlatform).crossType(CrossType.Pure).dependsOn(SafeBuffer)

organization in ThisBuild := "com.thoughtworks.binding"

publish / skip := true

enablePlugins(ScalaUnidocPlugin)

ScalaUnidoc / unidoc / unidocProjectFilter := {
  inAnyProject -- inProjects(SafeBuffer.jvm, Binding.jvm)
}

scalacOptions += "-Ymacro-annotations"

sys.env.get("GITHUB_REPOSITORY") match {
  case None =>
    Seq.empty
  case Some(slug) =>
    Seq(
      ThisBuild / homepage := Some(new URL("https", "github.com", raw"""/$slug""")),
      ThisBuild / scmInfo :=
        Some(
          ScmInfo(
            new URL("https", "github.com", raw"""/$slug"""),
            raw"""https://github.com/$slug.git""",
            Some(raw"""git@github.com:$slug.git""")
          )
        )
    )
}
