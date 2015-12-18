lazy val core = crossProject.crossType(CrossType.Pure)

lazy val dom = project dependsOn coreJS

lazy val coreJS = core.js.addSbtFiles(file("../build.sbt.shared"))

lazy val coreJVM = core.jvm.addSbtFiles(file("../build.sbt.shared"))

scalaVersion in ThisBuild := "2.11.7"

releaseUseGlobalVersion := true

releaseCrossBuild := false

releasePublishArtifactsAction := PgpKeys.publishSigned.value

publishTo := Some(if (isSnapshot.value) {
  "snapshots" at "http://nexus.delivery.realestate.com.au/nexus/content/repositories/snapshots"
} else {
  "releases" at "http://nexus.delivery.realestate.com.au/nexus/content/repositories/releases"
})
