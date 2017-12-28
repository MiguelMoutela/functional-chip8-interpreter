package de.malax.chip8.interpreters

import cats.effect.IO
import de.malax.chip8.algebra.System

object SystemInterpreter extends System[MachineStateIO] {
  def currentTimeMillis: MachineStateIO[Long] = IO { System.currentTimeMillis() }.liftIO()
}
