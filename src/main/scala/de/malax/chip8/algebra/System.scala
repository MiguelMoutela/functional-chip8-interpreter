package de.malax.chip8.algebra

import scala.language.higherKinds

trait System[F[_]] {
  def currentTimeMillis: F[Long]
}
