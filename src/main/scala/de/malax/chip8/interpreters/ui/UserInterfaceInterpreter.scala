package de.malax.chip8.interpreters
package ui

import java.awt.Color
import java.awt.event.KeyEvent
import java.awt.image.BufferedImage
import javax.swing.{ImageIcon, JFrame, JLabel}

import cats.effect.IO
import de.malax.chip8.algebra.UserInterface
import scodec.bits.BitVector

object UserInterfaceInterpreter
    extends UserInterface[MachineStateIO, BitVector] {
  private val displayPixelSize = 10
  private val window = new JFrame("Chip 8 Emulator")
  private val image = new BufferedImage(64 * displayPixelSize,
                                               32 * displayPixelSize,
                                               BufferedImage.TYPE_INT_RGB)
  private val label = new JLabel(new ImageIcon(image))
  window.getContentPane.add(label)
  window.setSize(800, 600)
  private val keyboardInput = new KeyboardInput()
  window.addKeyListener(keyboardInput)

  def init: MachineStateIO[Unit] = IO(window.setVisible(true)).liftIO()

  def update(x: Int, y: Int, displayInfo: BitVector): MachineStateIO[Unit] = IO {
    for (x <- 0 until 64; y <- 0 until 32) {
      for (iX <- 0 until displayPixelSize; iY <- 0 until displayPixelSize) {
        image.setRGB(x * displayPixelSize + iX, y * displayPixelSize + iY, if (displayInfo.get(y * 64 + x)) Color.GREEN.getRGB else Color.BLACK.getRGB)
      }
    }
    label.repaint()
  }.liftIO()

  val close: MachineStateIO[Unit] = IO { window.dispose() }.liftIO()

  def keyDown(keyCode: Int): MachineStateIO[Boolean] = IO {
    keyboardInput.poll()
    keyboardInput.keyDown(keyCode)
  }.liftIO()

  val beep: MachineStateIO[Unit] = IO { println("BEEP") }.liftIO()

  val escDown: MachineStateIO[Boolean] = keyDown(KeyEvent.VK_ESCAPE)
}
