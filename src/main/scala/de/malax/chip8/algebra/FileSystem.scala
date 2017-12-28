package de.malax.chip8.algebra

import scodec.bits.ByteVector
import scala.language.higherKinds

trait FileSystem[F[_]] {
  def getByteVector(filePath: String): F[ByteVector]
}
