package com.thoughtworks.binding

import javafx.application.Platform
import javax.swing.SwingUtilities

import org.scalatest._

import scala.collection.JavaConverters._

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class fxmlTest extends FreeSpec with Matchers with Inside {

  "Default property in Scene" in {
    @fxml val scene = {
      <?import javafx.scene.Scene?>
      <Scene>
        <?import javafx.scene.control.Button?>
        <Button></Button>
      </Scene>
    }
    scene.watch()
    scene.get should be(a[javafx.scene.Scene])
    scene.get.getRoot should be(a[javafx.scene.control.Button])
  }

  "root property in Scene" in {
    @fxml val scene = {
      <?import javafx.scene.Scene?>
      <Scene>
        <root>
          <?import javafx.scene.control.Button?>
          <Button></Button>
        </root>
      </Scene>
    }
    scene.watch()
    scene.get should be(a[javafx.scene.Scene])
    scene.get.getRoot should be(a[javafx.scene.control.Button])
  }

  "repeated property in Scene" in {
    @fxml val scene = {
      <?import javafx.scene.Scene?>
      <Scene>
        <?import javafx.scene.control.Button?>
        <Button fx:id="b"></Button>
        <stylesheets>
          {"a"}
          {b.toString}
          <String fx:value="c"/>
          {Seq("d","e")}
        </stylesheets>
      </Scene>
    }
    scene.watch()
    scene.get should be(a[javafx.scene.Scene])
    scene.get.getRoot should be(a[javafx.scene.control.Button])
    inside(scene.get.getStylesheets.asScala) {
      case Seq("a", s2, "c", "d", "e") =>
        s2 shouldNot be(empty)
    }
  }

  "HashMap" in {
    @fxml val hashMap = {
      type StringMap = java.util.HashMap[String, String]
      <StringMap foo="123" bar=""/>
    }
    hashMap.watch()
    hashMap.get.asScala should be(Map("foo" -> "123", "bar" -> ""))
  }

  "Nested builders" in {
    @fxml val scene = {
      <?import javafx.scene.Scene?>
      <Scene>
        <?import javafx.scene.control.Button?>
        <fill>
          <?import javafx.scene.paint.Color?>
          <Color>
            <red>1.0</red>
            <green>0.0</green>
            <blue>0.0</blue>
          </Color>
        </fill>
        <Button></Button>
      </Scene>
    }
    scene.watch()
    scene.get should be(a[javafx.scene.Scene])
    scene.get.getRoot should be(a[javafx.scene.control.Button])
    scene.get.getFill should be(javafx.scene.paint.Color.RED)
  }

  "fx:factory" in {
    @fxml val observableArrayList = {
      import javafx.collections.FXCollections
      <FXCollections fx:factory="observableArrayList">
        <String fx:value="A"/>
        <String fx:value="B"/>
        <String fx:value=""/>
      </FXCollections>
    }
    observableArrayList.watch()
    import scala.collection.JavaConverters._
    observableArrayList.get.asScala should be(Seq("A", "B", ""))
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

  "empty text for string property" in {
    @fxml val button = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <Button>
        <text></text>
      </Button>
    }
    button.watch()
    button.get.getText should be("")
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

  "Empty Button" in {
    @fxml val imageView = {
      <?import javafx.scene.control.*?>
      <Button></Button>
    }
  }

  "statie properties" in {
    import javafx.scene.control.Label
    import javafx.scene.layout.GridPane

    @fxml val gridPane = {
      <GridPane>
        <children>
          <Label text="My Label">
            <GridPane.rowIndex>{2}</GridPane.rowIndex>
            <GridPane.columnIndex>{3}</GridPane.columnIndex>
          </Label>
        </children>
      </GridPane>
    }
    gridPane.watch()
    inside(gridPane.get.getChildren.asScala) {
      case Seq(label: Label) =>
        GridPane.getRowIndex(label) should be(2)
        GridPane.getColumnIndex(label) should be(3)
    }

  }

  "fx:define" in {
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
      <VBox>
        <?import javafx.scene.control.Button?>
        <fx:define>
          <Button text="Hello" fx:id="b"></Button>
          <Button></Button>
        </fx:define>
        <Button>
          <fx:define>
            <String fx:id="space" fx:value=" "/>
          </fx:define>
          <text>
            <fx:define>
              <String fx:id="s" fx:value="World"/>
            </fx:define>
            {b.getText}
            {space}
            {s}
          </text>
        </Button>
        <Button>
          <text>
            {b.getText}
            {space}
            {s}
          </text>
        </Button>
      </VBox>
    }

    vbox.watch()

    inside(vbox.get.getChildren.asScala) {
      case Seq(button0: javafx.scene.control.Button, button1: javafx.scene.control.Button) => {
        button0 shouldNot be(button1)
        button0.getText should be("Hello World")
        button1.getText should be("Hello World")
      }
    }

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
