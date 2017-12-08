enablePlugins(Travis)

enablePlugins(SonatypeRelease)

lazy val secret = project.settings(publishArtifact := false).in {
  val secretDirectory = file(sourcecode.File()).getParentFile / "secret"
  org.apache.commons.io.FileUtils.deleteDirectory(secretDirectory)
  org.eclipse.jgit.api.Git
    .cloneRepository()
    .setURI("https://github.com/ThoughtWorksInc/tw-data-china-continuous-delivery-password.git")
    .setDirectory(secretDirectory)
    .setCredentialsProvider(
      new org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider(sys.env("GITHUB_PERSONAL_ACCESS_TOKEN"), "")
    )
    .call()
    .close()
  secretDirectory
}
