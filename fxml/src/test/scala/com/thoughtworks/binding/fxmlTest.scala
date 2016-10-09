package com.thoughtworks.binding

import javafx.application.Platform
import javafx.embed.swing.JFXPanel
import javax.swing.SwingUtilities

import org.scalatest._

import scala.collection.JavaConverters._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class fxmlTest extends FreeSpec with Matchers with Inside {


  "VBox of empty content" in {


    @fxml val vbox = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <VBox>
        <children>
          <Button>
            <text>My Button</text>
          </Button>
        </children>
      </VBox>
    }

    vbox.watch()

    inside(vbox.get.getChildren.asScala) {
      case Seq(button: javafx.scene.control.Button) =>
        button.getText should be("My Button")
    }

  }

  "simple VBox" in {
    @fxml val vbox = {
      import javafx.scene.layout.VBox
      <VBox></VBox>
    }

    vbox.watch()

    vbox.get should be(a[javafx.scene.layout.VBox])
  }

  override protected def withFixture(test: NoArgTest): Outcome = {
    val lock = new AnyRef
    @volatile var result: Option[Outcome] = None
    lock.synchronized {
      SwingUtilities.invokeLater(new Runnable {
        override def run(): Unit = {
          new javafx.embed.swing.JFXPanel
          Platform.runLater(new Runnable() {
            override def run(): Unit = {
              val outcome = fxmlTest.super.withFixture(test)
              lock.synchronized {
                result = Some(outcome)
                lock.notify()
              }
            }
          })
        }
      })
      while (result.isEmpty) {
        lock.wait()
      }
      result.get
    }
  }
}
