enablePlugins(Example)

libraryDependencies += "org.scalatest" %%% "scalatest" % "3.2.10" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "dsl" % "2.0.0-M2+228-ad681156"

libraryDependencies += "com.thoughtworks.dsl" %%% "reset" % "2.0.0-M2+228-ad681156" % Test

libraryDependencies += "com.thoughtworks.dsl" %%% "keywords-await" % "2.0.0-M2+228-ad681156" % Test

import meta._
exampleSuperTypes := {
  val (init"_root_.org.scalatest.freespec.AnyFreeSpec" +: traits) = exampleSuperTypes.value
  init"_root_.org.scalatest.freespec.AsyncFreeSpec" +: traits
}

exampleSuperTypes += init"_root_.org.scalatest.Inside"