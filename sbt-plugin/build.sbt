sbtPlugin := true

organization := "com.thoughtworks.binding"

name := "sbt-plugin"

description := "Sbt plug-in for RHTML."

releasePublishArtifactsAction <<= PgpKeys.publishSigned

addSbtPlugin("org.scala-js" % "sbt-scalajs" % "0.6.7")

scalaVersion := "2.10.6"

scriptedSettings

scriptedLaunchOpts := { scriptedLaunchOpts.value ++
  Seq("-Xmx1024M", "-XX:MaxPermSize=256M", "-Dplugin.version=" + version.value)
}

addSbtPlugin("com.typesafe.play" % "sbt-plugin" % "2.4.6")
