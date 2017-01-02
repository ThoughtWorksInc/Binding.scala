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

  "VBox that contains a Button" in {

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

  "VBox of empty content" in {
    @fxml val vbox = {
      import javafx.scene.layout.VBox
      <VBox></VBox>
    }

    vbox.watch()

    vbox.get should be(a[javafx.scene.layout.VBox])
  }

  "two VBoxes" in {
    @fxml val vbox = {
      import javafx.scene.layout.VBox
      <VBox></VBox>
        <VBox></VBox>
    }

    vbox.watch()

    inside(vbox.get.get) {
      case Seq(vbox0, vbox1) =>
        vbox0 should be(a[javafx.scene.layout.VBox])
        vbox1 should be(a[javafx.scene.layout.VBox])
    }
  }

  "wildcard import and a VBox" in {
    @fxml val vbox = {
      <?import javafx.scene.layout.*?>
        <VBox></VBox>
    }

    vbox.watch()

    vbox.get should be(a[javafx.scene.layout.VBox])
  }

  "import and a VBox" in {
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
        <VBox></VBox>
    }

    vbox.watch()

    vbox.get should be(a[javafx.scene.layout.VBox])
  }

  "import and two VBox" in {
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
        <VBox></VBox>
        <VBox></VBox>
    }

    vbox.watch()

    inside(vbox.get.get) {
      case Seq(vbox0, vbox1) =>
        vbox0 should be(a[javafx.scene.layout.VBox])
        vbox1 should be(a[javafx.scene.layout.VBox])
    }
  }

  "Nested import and a Button" in {
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
        <VBox>
          <children>
            <?import javafx.scene.control.Button?>
            <Button></Button>
          </children>
        </VBox>
    }

    vbox.watch()

    inside(vbox.get.getChildren.asScala) {
      case Seq(button) => {
        button should be(a[javafx.scene.control.Button])
      }
    }
  }

  "Nested import and a Button of default properties" in {
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
        <VBox>
          <?import javafx.scene.control.Button?>
          <Button></Button>
        </VBox>
    }

    vbox.watch()

    inside(vbox.get.getChildren.asScala) {
      case Seq(button) => {
        button should be(a[javafx.scene.control.Button])
      }
    }
  }

  "Nested import and multiple children of default properties" in {
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
        <VBox>
          <?import javafx.scene.control.Button?>
          <Button></Button>
          <Button></Button>
          <VBox></VBox>
        </VBox>
    }

    vbox.watch()

    inside(vbox.get.getChildren.asScala) {
      case Seq(button0, button1, button2) => {
        button0 should be(a[javafx.scene.control.Button])
        button1 should be(a[javafx.scene.control.Button])
        button2 should be(a[javafx.scene.layout.VBox])
        button0 shouldNot be(button1)
      }
    }
  }

  override protected def withFixture(test: NoArgTest): Outcome = {
    if (Platform.isFxApplicationThread) {
      fxmlTest.super.withFixture(test)
    } else {
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
}
