package de.malax.chip8

import cats.data.{EitherT, StateT}
import cats.effect.IO
import de.malax.chip8.model.{MachineState, Opcode}

package object interpreters {
  type MachineStateIO[A] = StateT[IO, MachineState, A]

  type ErrorOrIO[A] = EitherT[IO, Throwable, A]

  implicit class LiftIO[A](io: IO[A]) {
    def liftIO(): StateT[IO, MachineState, A] = StateT[IO, MachineState, A](s => io.map(a => (s,a)))
  }

  type BinaryToOpcodeMapper = Int => Option[Opcode]
  type VariablesToOpcodeMapper = Map[Char, Int] => Option[Opcode]
}
