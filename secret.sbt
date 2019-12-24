lazy val secret = {
  for (token <- sys.env.get("GITHUB_PERSONAL_ACCESS_TOKEN")) yield {
    val secret = project.settings(publish / skip := true).in {
      val secretDirectory = file(sourcecode.File()).getParentFile / "secret"
      IO.delete(secretDirectory)
      org.eclipse.jgit.api.Git
        .cloneRepository()
        .setURI("https://github.com/ThoughtWorksInc/tw-data-china-continuous-delivery-password.git")
        .setDirectory(secretDirectory)
        .setCredentialsProvider(
          new org.eclipse.jgit.transport.UsernamePasswordCredentialsProvider(token, "")
        )
        .call()
        .close()
      secretDirectory
    }
    secret
  }
}.getOrElse(null)
