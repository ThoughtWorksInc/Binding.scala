package com.thoughtworks.binding

import javafx.application.Application
import javafx.event.{ActionEvent, EventHandler, EventType}
import javafx.stage.{PopupWindow, Stage, Window, WindowEvent}

/**
  * @author 杨博 (Yang Bo) &lt;pop.atry@gmail.com&gt;
  */
final class Main extends Application {

  @fxml override def start(primaryStage: Stage): Unit = {

    import javafx.scene.layout.VBox
    import javafx.scene.control._
    import javafx.scene.Scene

    def newWindow(text: String) = {
      <Stage fx:id="myStage" title={text}>
        <scene>
          <Scene>
            <Label>{text}</Label>
          </Scene>
        </scene>
      </Stage>

      Binding {
        myStage.show()
      }.watch()
    }

    <Scene fx:id="myScene">
      <VBox>
        <Button onAction={ event: ActionEvent => newWindow("Newly created window")}>New Window</Button>
      </VBox>
    </Scene>

    Binding {
      primaryStage.setScene(myScene)
      primaryStage.show()
    }.watch()

  }
}

object Main {
  def main(args: Array[String]): Unit = {
    Application.launch(classOf[Main], args: _*)
  }
}
