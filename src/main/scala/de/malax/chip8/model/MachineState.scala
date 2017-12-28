package de.malax.chip8.model

import de.malax.chip8._
import scodec.bits._

case class MachineState private (
  pc: Int,
  display: BitVector,
  registers: Map[Register, Int],
  memory: ByteVector,
  stack: List[Int],
  randomSeed: Long,
  powerState: Boolean,
  timerAcc: Long,
  lastCycle: Long) {
  def withRegisterValue(register: Register, value: Int): MachineState = {
    copy(registers = registers.updated(register, value))
  }
}

object MachineState {
  val initial = MachineState(
    pc = 0x200,
    display = BitVector.fill(64 * 32)(high = false),
    registers = Register.all.map(r => r -> 0x00).toMap,
    memory = ByteVector
      .fill(0xFFF)(0x00)
      .patch(0, Chip8Constants.memoryInterpreterArea),
    stack = Nil,
    randomSeed = 0xCAFEBABE,
    powerState = true,
    timerAcc = 0L,
    lastCycle = 0L
  )
}
