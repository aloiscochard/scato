package scato
package io

trait App {
  def mainIO: IO[Unit]
  def rts: RTS = RTS.defaultRTS

  // TODO Feed arguments with the STM in IO? ... find something neat
  def main(args: Array[String]): Unit = rts.unsafePerformIO_(mainIO)
}
