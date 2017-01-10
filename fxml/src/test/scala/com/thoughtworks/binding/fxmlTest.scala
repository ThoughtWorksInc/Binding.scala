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

  "fx:factory" in {
    @fxml val observableArrayList = {
      import javafx.collections.FXCollections
      <FXCollections fx:factory="observableArrayList">
        <String fx:value="A"/>
        <String fx:value="B"/>
        <String fx:value="C"/>
      </FXCollections>
    }
    observableArrayList.watch()
    import scala.collection.JavaConverters._
    observableArrayList.get.asScala should be(Seq("A", "B", "C"))
  }

  "Reference outer element by fx:id" in {
    @fxml val button = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <Button fx:id="b">
        <text>{b.toString}</text>
      </Button>
    }

    button.watch()
    button.get.getText shouldNot be("")

  }

  "Reference sibling by fx:id" in {
    @fxml val buttons = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <Button fx:id="b" text={b.toString}/>
      <Button>
        <text>
          {b.getText}
        </text>
      </Button>
    }

    buttons.watch()
    inside(buttons.get) {
      case Seq(button1, button2) =>
        button1.getText shouldNot be("")
        button1.getText should be(button2.getText)
    }

  }

  "fx:value with fx:id" in {
    @fxml val button = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <Button>
        <text><String fx:id="s" fx:value="My Button"/></text>
      </Button>
    }

    button.watch()
    button.get.getText should be("My Button")

  }

  "spaces for string property" in {
    @fxml val button = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <Button>
        <text>   </text>
      </Button>
    }
    button.watch()
    button.get.getText should be("   ")
  }

  "spaces for boolean property" in {
    """
      @fxml val button = {
        import javafx.scene.control.Button
        <Button>
          <defaultButton>   </defaultButton>
        </Button>
      }
    """ shouldNot compile
  }

  "spaces for repeated property" in {
    @fxml val vbox = {
      import javafx.scene.layout.VBox
      <VBox>
        <children>   </children>
      </VBox>
    }
    vbox.watch()
    import scala.collection.JavaConverters._
    vbox.get.getChildren.asScala should be(empty)
  }

  "fx:value" in {
    @fxml val button = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <Button>
        <text><String fx:value="My Button"/></text>
      </Button>
    }

    button.watch()
    button.get.getText should be("My Button")

  }

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

    inside(vbox.get) {
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

    inside(vbox.get) {
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

  "Nested import and a dynamic Button in default properties" in {
    @fxml val vbox = {
      val button = {
        <?import javafx.scene.control.Button?>
        <Button></Button>
      }

      <?import javafx.scene.layout.VBox?>
      <VBox>
        {button.bind}
      </VBox>
    }

    vbox.watch()

    inside(vbox.get.getChildren.asScala) {
      case Seq(button) => {
        button should be(a[javafx.scene.control.Button])
      }
    }
  }

  "Nested @fxml annotation" in {
    @fxml val vbox = {
      @fxml val button = {
        <?import javafx.scene.control.Button?>
        <Button></Button>
      }

      <?import javafx.scene.layout.VBox?>
      <VBox>
        {button.bind}
      </VBox>
    }

    vbox.watch()

    inside(vbox.get.getChildren.asScala) {
      case Seq(button) => {
        button should be(a[javafx.scene.control.Button])
      }
    }
  }

  "Nested import and a dynamic Button in properties" in {
    @fxml val vbox = {
      val button = {
        <?import javafx.scene.control.Button?>
        <Button></Button>
      }

      <?import javafx.scene.layout.VBox?>
      <VBox>
        <children>
        {button.bind}
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

  "read-only Map properties" in {
    @fxml val button = {
      <?import javafx.scene.control.*?>
      <Button>
        <properties foo="123" bar="456"/>
      </Button>
    }
    button.watch()
    button.get should be(a[javafx.scene.control.Button])

    import scala.collection.JavaConverters._
    button.get.getProperties.asScala should be(Map("foo" -> "123", "bar" -> "456"))

  }

  "Location resolution" in {
    import javafx.scene.image._
    @fxml val imageView = {
      <ImageView>
        <image>
          <Image url={getClass.getResource("my_image.png")}/>
        </image>
      </ImageView>
    }
    imageView.watch()
    imageView.get.getImage.isError should be(false)
  }

  override protected def withFixture(test: NoArgTest): Outcome = {
    if (Platform.isFxApplicationThread) {
      fxmlTest. super.withFixture(test)
    } else {
      val lock = new AnyRef
      @volatile var result: Option[Outcome] = None
      lock.synchronized {
        SwingUtilities.invokeLater(new Runnable {
          override def run(): Unit = {
            new javafx.embed.swing.JFXPanel
            Platform.runLater(new Runnable() {
              override def run(): Unit = {
                val outcome = fxmlTest. super.withFixture(test)
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
