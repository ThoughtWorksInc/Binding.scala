enablePlugins(Travis)

enablePlugins(SonatypeRelease)

lazy val secret = project configure { secret =>
  sys.env.get("SECRET_GIT") match {
    case Some(gitUri) =>
      secret.addSbtFilesFromGit(gitUri, file("secret.sbt"))
    case None =>
      secret
  }
}
