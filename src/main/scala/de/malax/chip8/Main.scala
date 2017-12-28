package de.malax.chip8

import cats.data.EitherT
import cats.effect.IO
import cats.implicits._
import de.malax.chip8.interpreters._
import de.malax.chip8.interpreters.opcode.{OpcodeInterpreterImpl, OpcodeParserInterpreter}
import de.malax.chip8.interpreters.ui.UserInterfaceInterpreter
import de.malax.chip8.model.MachineState
import de.malax.chip8.programs.Programs

object Main extends App {
  val getFirstArg: ErrorOrIO[String] = EitherT {
    IO {
      args.head
    }
      .attempt
      .map(_.leftMap(ex => new Throwable("Please give a path to a Chip-8 ROM as the first argument!", ex)))
  }

  val ui = UserInterfaceInterpreter
  val machineInterpreter = new MachineInterpreter(ui, SystemInterpreter)
  val opcodeInterpreter = new OpcodeInterpreterImpl(machineInterpreter)

  val programs = new Programs(ui, machineInterpreter, OpcodeParserInterpreter, opcodeInterpreter)

  import FileSystemInterpreter._
  import programs._

  val runner = for {
    romFilePath <- getFirstArg
    rom <- getByteVector(romFilePath)
    _ <- EitherT(program(rom).runA(MachineState.initial).attempt)
  } yield ()

  val result = runner.value.unsafeRunSync()
  result.fold(ex => { println(ex); sys.exit(-1) }, _ => ())
}
