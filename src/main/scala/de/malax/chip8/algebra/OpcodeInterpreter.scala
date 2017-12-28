package de.malax.chip8.algebra

import scala.language.higherKinds

trait OpcodeInterpreter[F[_], Opcode] {
  def interpret(opcode: Opcode): F[Unit]
}
