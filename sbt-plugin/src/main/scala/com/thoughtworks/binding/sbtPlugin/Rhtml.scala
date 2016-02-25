package com.thoughtworks.binding.sbtPlugin

import sbt.AutoPlugin

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
object Rhtml extends AutoPlugin {

  override def requires = play.sbt.PlayScala

}
