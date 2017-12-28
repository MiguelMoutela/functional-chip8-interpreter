package de.malax.chip8.interpreters

import java.awt.event.KeyEvent

import cats.data.StateT._
import cats.effect.IO
import de.malax.chip8._
import de.malax.chip8.algebra.{Machine, System, UserInterface}
import de.malax.chip8.model._
import de.malax.chip8.utils.DisplayUtils
import scodec.bits.{BitVector, ByteVector}

final class MachineInterpreter(ui: UserInterface[MachineStateIO, BitVector],
                               timer: System[MachineStateIO])
    extends Machine[MachineStateIO, ByteVector, Key, Register] {

  import timer._
  import ui._

  def checkForEsc: MachineStateIO[Unit] = {
    for {
      esc <- escDown
      _ <- if (esc) {
        modify[IO, MachineState](_.copy(powerState = false))
          .flatMap(_ => ui.close)
      } else {
        pure[IO, MachineState, Unit](())
      }
    } yield ()
  }

  def updateRegisters: MachineStateIO[Unit] = {
    for {
      time <- currentTimeMillis
      lastCycle <- inspect[IO, MachineState, Long](_.lastCycle)
      timePassed = time - lastCycle
      _ <- modify[IO, MachineState](m =>
        m.copy(timerAcc = m.timerAcc + timePassed))
      dTValue <- inspect[IO, MachineState, Int](_.registers(DT))
      sTValue <- inspect[IO, MachineState, Int](_.registers(ST))
      timerAcc <- inspect[IO, MachineState, Long](_.timerAcc)
      _ <- if (timerAcc > 1000 / 60) {
        for {
          _ <- modify[IO, MachineState](m =>
            m.copy(timerAcc = m.timerAcc + (1000 / 60)))
          _ <- if (dTValue > 0) {
            modify[IO, MachineState](m =>
              m.copy(registers = m.registers.updated(DT, dTValue - 1)))
          } else { pure[IO, MachineState, Unit](()) }
          _ <- if (sTValue > 0) {
            for {
              _ <- modify[IO, MachineState](m =>
                m.copy(registers = m.registers.updated(ST, sTValue - 1)))
              _ <- beep
            } yield ()
          } else { pure[IO, MachineState, Unit](()) }
        } yield ()
      } else {
        pure[IO, MachineState, Unit](())
      }
    } yield ()
  }

  val checkEscAndUpdateRegisters = for {
    _ <- checkForEsc
    _ <- updateRegisters
  } yield ()

  def patchMemory(address: Int, bytes: ByteVector) =
    for {
      _ <- checkEscAndUpdateRegisters
      _ <- modify[IO, MachineState](m =>
        m.copy(memory = m.memory.patch(address, bytes)))
    } yield ()

  val clearScreen = for {
    _ <- checkEscAndUpdateRegisters
    _ <- modify[IO, MachineState](m =>
      m.copy(display = BitVector.fill(m.display.length)(high = false)))
  } yield ()

  def pushStack(address: Int): MachineStateIO[Unit] =
    for {
      _ <- checkEscAndUpdateRegisters
      _ <- modify[IO, MachineState](m => m.copy(stack = address :: m.stack))
    } yield ()

  val popStack: MachineStateIO[Int] =
    for {
      _ <- checkEscAndUpdateRegisters
      stack <- inspect[IO, MachineState, List[Int]](_.stack)
      h :: t = stack
      _ <- modify[IO, MachineState](m => m.copy(stack = t))
    } yield h

  val generateRandom: MachineStateIO[Int] =
    for {
      _ <- checkEscAndUpdateRegisters
      rndInt <- IO(math.random().toInt & 0xFF).liftIO()
    } yield rndInt

  def drawSprite(x: Int, y: Int, bytes: ByteVector): MachineStateIO[Boolean] =
    for {
      _ <- checkEscAndUpdateRegisters
      display <- inspect[IO, MachineState, BitVector](_.display)
      (newDisplay, collision) = DisplayUtils.drawSprite(x,
                                                        y,
                                                        bytes,
                                                        64,
                                                        32,
                                                        display)
      _ <- update(x, y, newDisplay)
      _ <- modify[IO, MachineState](m => m.copy(display = newDisplay))
    } yield collision

  def getKeyDown(key: Key): MachineStateIO[Boolean] =
    for {
      _ <- checkEscAndUpdateRegisters
      keyDown <- ui.keyDown(keyMapping(key))
    } yield keyDown

  def readByte(address: Int): MachineStateIO[Int] =
    for {
      _ <- checkEscAndUpdateRegisters
      b <- inspect[IO, MachineState, Int](_.memory.get(address) & 0xFF)
    } yield b

  def writeByte(address: Int, byte: Int): MachineStateIO[Unit] =
    for {
      _ <- checkEscAndUpdateRegisters
      _ <- modify[IO, MachineState](m => m.copy(memory = m.memory.splice(address, ByteVector(byte))))
    } yield ()

  def readRegister(register: Register): MachineStateIO[Int] =
    for {
      _ <- checkEscAndUpdateRegisters
      r <- inspect[IO, MachineState, Int](_.registers(register))
    } yield r

  def writeRegister(register: Register, byte: Int): MachineStateIO[Unit] =
    for {
      _ <- checkEscAndUpdateRegisters
      _ <- modify[IO, MachineState](m => m.copy(registers = m.registers.updated(register, byte)))
    } yield ()

  val readPc: MachineStateIO[Int] =
    for {
      pc <- inspect[IO, MachineState, Int](_.pc)
    } yield pc

  def writePc(address: Int): MachineStateIO[Unit] =
    for {
      _ <- modify[IO, MachineState](m => m.copy(pc = address))
    } yield ()

  val readPowerState: MachineStateIO[Boolean] =
    for {
      _ <- checkEscAndUpdateRegisters
      ps <- inspect[IO, MachineState, Boolean](_.powerState)
    } yield ps

  def incrementProgramCounter(by: Int): MachineStateIO[Unit] =
    readPc.flatMap(pc => writePc(pc + by))

  private val keyMapping = Map[Key, Int](
    Key1 -> KeyEvent.VK_1,
    Key2 -> KeyEvent.VK_2,
    Key3 -> KeyEvent.VK_3,
    KeyC -> KeyEvent.VK_4,
    Key4 -> KeyEvent.VK_Q,
    Key5 -> KeyEvent.VK_W,
    Key6 -> KeyEvent.VK_E,
    KeyD -> KeyEvent.VK_R,
    Key7 -> KeyEvent.VK_A,
    Key8 -> KeyEvent.VK_S,
    Key9 -> KeyEvent.VK_D,
    KeyE -> KeyEvent.VK_F,
    KeyA -> KeyEvent.VK_Y,
    Key0 -> KeyEvent.VK_X,
    KeyB -> KeyEvent.VK_C,
    KeyF -> KeyEvent.VK_V
  )
}
