package de.malax.chip8.programs

import cats._
import cats.implicits._
import de.malax.chip8.algebra._

import scala.language.higherKinds

final class Programs[F[_]: Monad,
                     DisplayInfo,
                     ByteVector,
                     Key,
                     Register,
                     Opcode](
    ui: UserInterface[F, DisplayInfo],
    machine: Machine[F, ByteVector, Key, Register],
    opcodeParer: OpcodeParser[Opcode],
    opcodeInterpreter: OpcodeInterpreter[F, Opcode]
) {
  import machine._
  import opcodeInterpreter._
  import opcodeParer._

  val cycle: F[Boolean] = for {
    pc <- readPc
    opcodeByte <- Monad[F].map2(readByte(pc), readByte(pc + 1))((hi, lo) =>
      hi << 8 | lo)
    _ <- parse(opcodeByte).map(interpret).getOrElse(Monad[F].unit)
    powerState <- readPowerState
  } yield powerState

  def program(rom: ByteVector): F[Unit] =
    for {
      _ <- ui.init
      _ <- patchMemory(0x200, rom)
      _ <- Monad[F].iterateUntil(cycle)(powerState => !powerState)
    } yield ()
}
