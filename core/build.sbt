organization := "au.com.realcommercial"

name := "functional-data-binding-demo-core"

scalaVersion := "2.11.7"

libraryDependencies += "com.thoughtworks.each" %%% "each" % "0.4.2"

libraryDependencies += "com.lihaoyi" %%% "utest" % "0.3.1" % Test

testFrameworks += new TestFramework("utest.runner.Framework")