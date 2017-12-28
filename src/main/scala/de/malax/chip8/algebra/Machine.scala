package de.malax.chip8.algebra

import scala.language.higherKinds

trait Machine[F[_], ByteVector, Key, Register] {
  def patchMemory(address: Int, bytes: ByteVector): F[Unit]
  val clearScreen: F[Unit]
  def pushStack(address: Int): F[Unit]
  val popStack: F[Int]
  val generateRandom: F[Int]
  def drawSprite(x: Int, y: Int, bytes: ByteVector): F[Boolean]
  def getKeyDown(key: Key): F[Boolean]
  def readByte(address: Int): F[Int]
  def writeByte(address: Int, byte: Int): F[Unit]
  def readRegister(register: Register): F[Int]
  def writeRegister(register: Register, byte: Int): F[Unit]
  val readPc: F[Int]
  def writePc(address: Int): F[Unit]
  val readPowerState: F[Boolean]
  def incrementProgramCounter(by: Int = 2): F[Unit]
}
