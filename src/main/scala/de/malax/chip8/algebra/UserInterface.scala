package de.malax.chip8.algebra

import scala.language.higherKinds

trait UserInterface[F[_], DisplayInfo] {
  def init: F[Unit]
  def update(x: Int, y: Int, displayInfo: DisplayInfo): F[Unit]
  val close: F[Unit]
  def keyDown(keyCode: Int): F[Boolean]
  val escDown: F[Boolean]
  val beep: F[Unit]
}
