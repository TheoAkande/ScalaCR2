package editer

import CR2Reader.readBinaryFile

import scalafx.application.{JFXApp3, Platform}
import scalafx.beans.property.{IntegerProperty, ObjectProperty}
import scalafx.scene.Scene
import scalafx.scene.paint.Color
import scalafx.scene.paint.Color._
import scalafx.scene.shape.Rectangle
import scalafx.scene.image._

import scala.concurrent.Future
import scala.util.Random
import scala.concurrent.ExecutionContext.Implicits.global

@main def go = readBinaryFile()

// object EditFx extends JFXApp3 {
//   override def start(): Unit = {
//     stage = new JFXApp3.PrimaryStage {
//       title = "Edit"
//       scene = new Scene(600, 600) {
//         val image = new Image("file:C:\\Users\\theoa\\Editing\\edit\\resources\\bird.CR2")
//         val view = new ImageView(image)
//         content = view
//       }
//     }
//   }
// }