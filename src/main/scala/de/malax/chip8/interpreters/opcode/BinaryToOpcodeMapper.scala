package de.malax.chip8.interpreters.opcode

import de.malax.chip8.interpreters.{BinaryToOpcodeMapper, VariablesToOpcodeMapper}
import de.malax.chip8.utils.BinaryPattern

object BinaryToOpcodeMapper {
  def build(pattern: String)(variablesToOpcode: VariablesToOpcodeMapper): BinaryToOpcodeMapper = {
    val matchBinary = BinaryPattern.compileNibblePatternFromString(pattern).matchBinary(_)
    matchBinary.andThen(_.flatMap(variablesToOpcode))
  }
}
