package com.thoughtworks.binding

import scala.annotation.unchecked.uncheckedVariance
import scalaz.Free

type Binding[+A] = Observable.BehaviorSubject[A]