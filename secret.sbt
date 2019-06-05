lazy val secret = {
  for (gist <- sys.env.get("SECRET_GIST")) yield {
    val secret = project.settings(publishArtifact := false).in {
      val secretDirectory = file(sourcecode.File()).getParentFile / "secret"
      IO.delete(secretDirectory)
      org.eclipse.jgit.api.Git
        .cloneRepository()
        .setURI(gist)
        .setDirectory(secretDirectory)
        .call()
        .close()
      secretDirectory
    }
    secret
  }
}.getOrElse(null)
