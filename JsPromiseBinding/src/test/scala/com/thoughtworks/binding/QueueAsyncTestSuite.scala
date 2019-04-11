package com.thoughtworks.binding

/**
  * @author 杨博 (Yang Bo)
  */
trait QueueAsyncTestSuite extends org.scalatest.AsyncTestSuite {
  override implicit def executionContext = scala.scalajs.concurrent.JSExecutionContext.queue
}
