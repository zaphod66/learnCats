package com.zaphod.stuff

import scala.swing._

object ScalaSwingTest extends SimpleSwingApplication {
  val textArea = new TextArea("Hello, Scala-Swing world")
  val scrollPane = new ScrollPane(textArea)

  override def top: Frame = new MainFrame {
    title = "Hello, World!"
    contents = scrollPane
    size = new Dimension(600, 400)
    centerOnScreen()
  }
}
