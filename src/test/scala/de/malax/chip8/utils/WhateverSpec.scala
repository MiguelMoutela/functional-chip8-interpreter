package de.malax.chip8.utils

import cats.effect.IO
import de.malax.chip8.algebra.UserInterface
import de.malax.chip8.interpreters.{_}
import de.malax.chip8.model._
import de.malax.chip8.interpreters.opcode._
import org.scalatest.{FlatSpec, Matchers}
import scodec.bits.BitVector

object UiStub extends UserInterface[MachineStateIO, BitVector] {
  override def init = IO(()).liftIO()

  override def update(x: Int, y: Int, displayInfo: BitVector) = IO(()).liftIO()

  override val close = IO(()).liftIO()

  override def keyDown(e: Int) = IO(false).liftIO()

  override val beep = IO(()).liftIO()

  override val escDown = IO(false).liftIO()
}

class WhateverSpec extends FlatSpec with Matchers {

  val machineInterpreter = new MachineInterpreter(UiStub, SystemInterpreter)
  val opcodeMapper = new OpcodeInterpreterImpl(machineInterpreter)

  "PureCompiler" should "execute ClearScreen" in {
    val m = execute(opcodeMapper.interpret(ClearScreen), MachineState.initial)
    m.display should be (BitVector.fill(64 * 32)(high = false))
  }

  it should "execute Return" in {
    val machineState = MachineState.initial.copy(stack = List(0xAAA, 0xBBB))
    val m = execute(opcodeMapper.interpret(Return), machineState)
    m.stack should be (List(0xBBB))
    m.pc should be (0xAAA)
  }

  it should "execute JumpToAddress" in {
    val m = execute(opcodeMapper.interpret(JumpToAddress(0xABC)), MachineState.initial)
    m.pc should be (0xABC)
  }

  it should "execute CallSubroutine" in {
    val machineState = MachineState.initial.copy(stack = List(0xAAA, 0xBBB), pc = 0x222)
    val m = execute(opcodeMapper.interpret(CallSubroutine(0xABC)), machineState)

    m.pc should be (0xABC)
    m.stack.head should be (0x224)
  }

  it should "execute successful SkipIfEqual" in {
    val machineState = MachineState.initial.copy(stack = List(0xAAA, 0xBBB), pc = 0x222).withRegisterValue(VB, 0xFF)
    val m = execute(opcodeMapper.interpret(SkipIfEqual(VB, 0xFF)), machineState)

    m.copy(timerAcc = 0L) should be (machineState.copy(pc = 0x226, timerAcc = 0L))
  }

  it should "execute unsuccessful SkipIfEqual" in {
    val machineState = MachineState.initial.copy(stack = List(0xAAA, 0xBBB), pc = 0x222).withRegisterValue(VB, 0xF0)
    val m = execute(opcodeMapper.interpret(SkipIfEqual(VB, 0xFF)), machineState)

    m.copy(timerAcc = 0L) should be (machineState.copy(pc = 0x224, timerAcc = 0L))
  }

  it should "execute AddRegister with overflow correctly" in {
    val machine = MachineState.initial.withRegisterValue(VA, 0x3E).withRegisterValue(VB, 0xFF)
    val m = execute(opcodeMapper.interpret(AddRegister(VA, VB)), machine)
    m.copy(timerAcc = 0L) should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x3D).withRegisterValue(VF, 0x01))
  }

  it should "execute AddRegister without overflow correctly" in {
    val machine = MachineState.initial.withRegisterValue(VA, 0x0A).withRegisterValue(VB, 0x01)
    val m = execute(opcodeMapper.interpret(AddRegister(VA, VB)), machine)
    m.copy(timerAcc = 0L) should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x0B).withRegisterValue(VF, 0x00))
  }

  it should "execute AddConstant with overflow correctly" in {
    val machine = MachineState.initial.withRegisterValue(VA, 0x0A)
    val m = execute(opcodeMapper.interpret(AddConstant(VA, 0xFF)), machine)
    m.copy(timerAcc = 0L) should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x0A))
  }

  it should "execute AddConstant without overflow correctly" in {
    val machine = MachineState.initial.withRegisterValue(VA, 0x0A)
    val m = execute(opcodeMapper.interpret(AddConstant(VA, 0x02)), machine)
    m.copy(timerAcc = 0L) should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x0C))
  }

  it should "execute SubtractRegister with borrow correctly" in {
    val machine = MachineState.initial.withRegisterValue(VA, 0x01).withRegisterValue(VB, 0x02)
    val m = execute(opcodeMapper.interpret(SubtractRegister(VA, VB)), machine)
    m.copy(timerAcc = 0L) should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0xFF).withRegisterValue(VF, 0x00))
  }

  it should "execute ShiftLeft correctly" in {
    val machine = MachineState.initial.withRegisterValue(VA, 0x00).withRegisterValue(VB, 0xFF)
    val m = execute(opcodeMapper.interpret(ShiftLeft(VA, VB)), machine)
    m.copy(timerAcc = 0L) should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0xFE).withRegisterValue(VF, 0x01))
  }

  it should "execute RightLeft correctly" in {
    val machine = MachineState.initial.withRegisterValue(VA, 0x00).withRegisterValue(VB, 0xFF)
    val m = execute(opcodeMapper.interpret(ShiftRight(VA, VB)), machine)
    m.copy(timerAcc = 0L) should be (machine.copy(pc = machine.pc + 2).withRegisterValue(VA, 0x7F).withRegisterValue(VF, 0x01))
  }

  private def execute[A](p: MachineStateIO[A], m: MachineState): MachineState = {
    p.runS(m).unsafeRunSync()
  }
}
