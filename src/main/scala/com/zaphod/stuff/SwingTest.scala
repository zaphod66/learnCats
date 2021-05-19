package com.zaphod.stuff

import java.awt.BorderLayout
import java.awt.Dimension
import javax.swing.{JFrame, JScrollPane, JTextArea, WindowConstants}

object SwingTest extends App {

  val textArea = new JTextArea
  textArea.setText("Hello, Swing world")
  val scrollPane = new JScrollPane(textArea)

  val frame = new JFrame("Hello, Swing")
  frame.getContentPane.add(scrollPane, BorderLayout.CENTER)
  frame.setDefaultCloseOperation(WindowConstants.EXIT_ON_CLOSE)
  frame.setSize(new Dimension(600, 400))
  frame.setLocationRelativeTo(null)
  frame.setVisible(true)
}
