package au.com.realcommercial.bindingScala

class Publisher[Subscriber] extends collection.mutable.HashMap[Subscriber, Int] {

  override def default(subscriber: Subscriber) = 0

  def subscribe(subscriber: Subscriber): Unit = {
    val oldValue = this (subscriber)
    if (oldValue < 0) {
      throw new IllegalStateException()
    }
    val newValue = oldValue + 1
    this (subscriber) = newValue
  }

  def unsubscribe(subscriber: Subscriber): Unit = {
    val oldValue = this (subscriber)
    if (oldValue <= 0) {
      throw new IllegalStateException()
    }
    val newValue = oldValue - 1
    if (newValue == 0) {
      this -= subscriber
    } else {
      this (subscriber) = newValue
    }
  }

}
