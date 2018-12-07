enablePlugins(Travis)

enablePlugins(SonatypeRelease)

releaseProcess := {
  val stepIndex = releaseProcess.value.indexOf(sbtrelease.ReleaseStateTransformations.publishArtifacts)
  val step = releaseStepCommand(Sonatype.SonatypeCommand.sonatypeOpen, s" ${version.value}")
  releaseProcess.value.patch(stepIndex, Seq[ReleaseStep](step), 0)
}

lazy val secret = project settings(publishArtifact := false) configure { secret =>
  sys.env.get("GITHUB_PERSONAL_ACCESS_TOKEN") match {
    case Some(pat) =>
      import org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider
      secret.addSbtFilesFromGit(
        "https://github.com/ThoughtWorksInc/tw-data-china-continuous-delivery-password.git",
        new UsernamePasswordCredentialsProvider(pat, ""),
        file("secret.sbt"))
    case None =>
      secret
  }
}
