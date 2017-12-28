package de.malax.chip8.algebra

trait OpcodeParser[Opcode] {
  def parse(opcode: Int): Option[Opcode]
}
