val disableDeploySh = TaskKey[Unit]("disable-deploy-sh", "Rename deploy.sh to deploy.sh.disabled.")

disableDeploySh := {
  val log = (streams in disableDeploySh).value.log
  Process(
    "git" :: "mv" :: "--" :: "deploy.sh" :: "deploy.sh.disabled" :: Nil,
    baseDirectory.value
  ) ! log
}

import ReleaseTransformations._

releaseProcess := {
  releaseProcess.value.patch(releaseProcess.value.indexOf(commitReleaseVersion), Seq[ReleaseStep](releaseStepTask(disableDeploySh)), 0)
}

pgpSecretRing := baseDirectory.value / "secret" / "secring.asc"

pgpPublicRing := baseDirectory.value / "pubring.asc"

pgpPassphrase := Some(Array.empty)
