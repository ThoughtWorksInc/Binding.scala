organization := "au.com.realcommercial"

name :=  organization.value + "functional-data-binding-demo-core"

scalaVersion := "2.11.7"

libraryDependencies += "org.scalaz" %% "scalaz-core" % "7.1.3"

libraryDependencies += "org.specs2" %% "specs2-junit" % "3.6.4" % Test

libraryDependencies += "org.specs2" %% "specs2-core" % "3.6.4" % Test

libraryDependencies += "org.specs2" %% "specs2-matcher-extra" % "3.6.4" % Test

libraryDependencies += "com.thoughtworks.each" %% "each" % "0.4.0"

libraryDependencies += "org.scala-lang" % "scala-reflect" % scalaVersion.value
