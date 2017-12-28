package de.malax.chip8.interpreters

import java.io.{File, FileInputStream}

import cats.data.EitherT
import cats.effect.IO
import de.malax.chip8.algebra.FileSystem
import org.apache.commons.io.IOUtils
import scodec.bits.ByteVector

object FileSystemInterpreter extends FileSystem[ErrorOrIO] {
  def getByteVector(filePath: String): ErrorOrIO[ByteVector] = EitherT {
    IO {
      ByteVector(IOUtils.toByteArray(new FileInputStream(new File(filePath))))
    }.attempt
  }
}
