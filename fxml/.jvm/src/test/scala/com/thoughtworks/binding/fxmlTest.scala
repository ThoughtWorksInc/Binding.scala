package com.thoughtworks.binding

import javafx.application.Platform
import javax.swing.SwingUtilities

import com.thoughtworks.binding.Binding.{BindingSeq, Var, Vars, Constants}
import org.scalatest._

import scala.collection.JavaConverters._
import scala.collection.mutable

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
    scene.value should be(a[javafx.scene.Scene])
    scene.value.getRoot should be(a[javafx.scene.control.Button])
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
    scene.value should be(a[javafx.scene.Scene])
    scene.value.getRoot should be(a[javafx.scene.control.Button])
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
    scene.value should be(a[javafx.scene.Scene])
    scene.value.getRoot should be(a[javafx.scene.control.Button])
    inside(scene.value.getStylesheets.asScala) {
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
    hashMap.value.asScala should be(Map("foo" -> "123", "bar" -> ""))
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
    scene.value should be(a[javafx.scene.Scene])
    scene.value.getRoot should be(a[javafx.scene.control.Button])
    scene.value.getFill should be(javafx.scene.paint.Color.RED)
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
    observableArrayList.value.asScala should be(Seq("A", "B", ""))
  }

  "Reference outer element by fx:id" in {
    @fxml val pair = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <Button fx:id="b">
        <text>{b.toString}</text>
      </Button>
      (Binding(b), Binding(b))
    }

    inside(pair) {
      case (button, button2) =>
        button.watch()
        button.value.getText shouldNot be("")
        button2.watch()
        button2.value.getText shouldNot be("")
    }

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
    inside(buttons.value) {
      case Seq(button1, button2) =>
        button1.getText shouldNot be("")
        button1.getText should be(button2.getText)
    }
  }

  "reference by both val and fx:id" in {
    @fxml val pair = {
      val b1 = <?import javafx.scene.control.Button?><Button fx:id="b2"/>
      (b1, Binding(b2))
    }
    pair._1.watch()
    pair._2.watch()
    pair._1.value should be(pair._2.value)
  }

  "Pattern matching" in {
    @fxml val (buttons, 2) = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      (
        <Button fx:id="b" text={b.toString}/>
        <Button>
          <text>
            {b.getText}
          </text>
        </Button>,
        2
      )
    }

    buttons.watch()
    inside(buttons.value) {
      case Seq(button1, button2) =>
        button1.getText shouldNot be("")
        button1.getText should be(button2.getText)
    }

  }

  "fx:value with fx:id" in {
    @fxml val bs = {
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <Button fx:id="b">
        <text><String fx:id="s" fx:value="My Button"/></text>
      </Button>

      Binding((b, s))
    }

    bs.watch()
    inside(bs.value) {
      case (b, s) =>
        b.getText should be("My Button")
        s should be("My Button")
    }
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
    button.value.getText should be("   ")
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
    button.value.getText should be("")
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
    vbox.value.getChildren.asScala should be(empty)
  }

  "Constrants for repeated property" in {
    @fxml val vbox = {
      import javafx.scene.layout.VBox
      <VBox>
        <children>{
          Constants()
        }</children>
      </VBox>
    }
    vbox.watch()
    import scala.collection.JavaConverters._
    vbox.value.getChildren.asScala should be(empty)
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
    button.value.getText should be("My Button")

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

    inside(vbox.value.getChildren.asScala) {
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

    vbox.value should be(a[javafx.scene.layout.VBox])
  }

  "two VBoxes" in {
    import javafx.scene.layout.VBox
    @fxml val vbox: BindingSeq[VBox] = {
      <VBox></VBox>
      <VBox></VBox>
    }

    vbox.watch()

    inside(vbox.value) {
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

    vbox.value should be(a[javafx.scene.layout.VBox])
  }

  "import and a VBox" in {
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
      <VBox></VBox>
    }

    vbox.watch()

    vbox.value should be(a[javafx.scene.layout.VBox])
  }

  "import and two VBox" in {
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
      <VBox></VBox>
      <VBox></VBox>
    }

    vbox.watch()

    inside(vbox.value) {
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

    inside(vbox.value.getChildren.asScala) {
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

    inside(vbox.value.getChildren.asScala) {
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

    inside(vbox.value.getChildren.asScala) {
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

    inside(vbox.value.getChildren.asScala) {
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

    inside(vbox.value.getChildren.asScala) {
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

    inside(vbox.value.getChildren.asScala) {
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
    button.value should be(a[javafx.scene.control.Button])

    import scala.collection.JavaConverters._
    button.value.getProperties.asScala should be(Map("foo" -> "123", "bar" -> "456"))

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
    imageView.value.getImage.isError should be(false)
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
    inside(gridPane.value.getChildren.asScala) {
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

    inside(vbox.value.getChildren.asScala) {
      case Seq(button0: javafx.scene.control.Button, button1: javafx.scene.control.Button) => {
        button0 shouldNot be(button1)
        button0.getText should be("Hello World")
        button1.getText should be("Hello World")
      }
    }

  }

  "onAction" in {
    val buttonText = Var("Click Me!")
    import javafx.event.ActionEvent
    import javafx.event.EventHandler
    val handleButtonAction = new EventHandler[ActionEvent] {
      override def handle(event: ActionEvent) = {
        buttonText.value = "Clicked"
      }
    }
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
      <VBox>
        <children>
          <?import javafx.scene.control.Button?>
          <Button text={buttonText.bind} onAction={handleButtonAction}/>
        </children>
      </VBox>
    }

    vbox.watch()
    inside(vbox.value.getChildren.asScala) {
      case Seq(button: javafx.scene.control.Button) =>
        button.getText should be("Click Me!")
        button.getOnAction.handle(new ActionEvent)
        button.getText should be("Clicked")
    }

  }

  "inline onAction" in {
    val buttonText = Var("Click Me!")
    import javafx.event.ActionEvent
    import javafx.event.EventHandler
    @fxml val vbox = {
      <?import javafx.scene.layout.VBox?>
      <VBox>
        <children>
          <?import javafx.scene.control.Button?>
          <Button text={buttonText.bind} onAction={_: ActionEvent => buttonText.value = "Clicked"}/>
        </children>
      </VBox>
    }

    vbox.watch()
    inside(vbox.value.getChildren.asScala) {
      case Seq(button: javafx.scene.control.Button) =>
        button.getText should be("Click Me!")
        button.getOnAction.handle(new ActionEvent)
        button.getText should be("Clicked")
    }

  }

  "onChange" in {
    import javafx.collections.ListChangeListener
    import javafx.scene.Node
    import javafx.scene.layout.VBox
    import javafx.scene.control.Button

    val eventHandlers = mutable.Queue.empty[ListChangeListener.Change[_ <: Node] => Assertion]
    val handler = new ListChangeListener[Node] {
      override def onChanged(c: ListChangeListener.Change[_ <: Node]): Unit = {
        while (c.next()) {
          eventHandlers.dequeue().apply(c)
        }
      }
    }
    val buttonTexts = Vars.empty[String]

    @fxml val vbox = {
      <VBox>
        <children onChange={handler}>
          <Button text="first button"/>
          {
            for (t <- buttonTexts) yield {
              <Button text={t}/>
            }
          }
          <Button text="last button"/>
        </children>
      </VBox>
    }
    eventHandlers should be(empty)

    eventHandlers.enqueue { c =>
      c.getRemovedSize should be(0)
      c.getFrom should be(0)
      inside(c.getAddedSubList.asScala) {
        case Seq(button1: Button, button2: Button) =>
          button1.getText should be("first button")
          button2.getText should be("last button")
      }
    }
    vbox.watch()
    inside(vbox.value.getChildren.asScala) {
      case Seq(b0: Button, b1: Button) =>
        b0.getText should be("first button")
        b1.getText should be("last button")
    }
    eventHandlers should be(empty)

    eventHandlers.enqueue { c =>
      c.getRemovedSize should be(0)
      c.getFrom should be(1)
      inside(c.getAddedSubList.asScala) {
        case Seq(button1: Button, button2: Button, button3: Button) =>
          button1.getText should be("foo")
          button2.getText should be("bar")
          button3.getText should be("baz")
      }
    }

    buttonTexts.value ++= Seq("foo", "bar", "baz")

    eventHandlers should be(empty)

    inside(vbox.value.getChildren.asScala) {
      case Seq(b0: Button, b1: Button, b2: Button, b3: Button, b4: Button) =>
        b0.getText should be("first button")
        b1.getText should be("foo")
        b2.getText should be("bar")
        b3.getText should be("baz")
        b4.getText should be("last button")
    }

  }

  "inline onChange" in {
    @fxml val vbox = {
      import javafx.collections.ListChangeListener
      import javafx.scene.Node
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      <VBox>
        <children onChange={event: ListChangeListener.Change[_ <: Node] =>}>
        </children>
      </VBox>
    }
    // should compile
  }

  "onParentChange" in {
    @fxml val vbox = {
      import javafx.collections._
      import javafx.scene.Node
      import javafx.scene.layout.VBox
      import javafx.scene.control.Button
      import javafx.beans.value._
      import javafx.scene.Parent
      <VBox>
        <Button onParentChange={ (event: ObservableValue[_ <: Parent], oldValue:Parent, newValue:Parent) => println(event)}/>
      </VBox>
    }
    vbox.watch()
    // should compile
  }

  "entity reference" in {
    import javafx.scene.control.Button
    @fxml val button = <Button id="&lt; &lt; ">&gt; &gt;</Button>
    button.watch()
    button.value.getId should be("< < ")
    button.value.getText should be(">>")
  }

  override protected def withFixture(test: NoArgTest): Outcome = {

    if (SwingUtilities.isEventDispatchThread) {
      new javafx.embed.swing.JFXPanel
    } else {
      val lock = new AnyRef
      @volatile var initialized = false
      lock.synchronized {
        SwingUtilities.invokeLater(new Runnable {
          override def run(): Unit = {
            new javafx.embed.swing.JFXPanel
            lock.synchronized {
              initialized = true
              lock.notify()
            }
          }
        })
        while (!initialized) {
          lock.wait()
        }
      }
    }

    if (Platform.isFxApplicationThread) {
      fxmlTest.super.withFixture(test)
    } else {
      {
        val lock = new AnyRef
        @volatile var result: Option[Outcome] = None
        lock.synchronized {
          Platform.runLater(new Runnable() {
            override def run(): Unit = {
              val outcome = fxmlTest.super.withFixture(test)
              lock.synchronized {
                result = Some(outcome)
                lock.notify()
              }
            }
          })
          while (result.isEmpty) {
            lock.wait()
          }
        }
        result.get
      }
    }
  }
}
