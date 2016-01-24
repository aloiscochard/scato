package scato
package io

import scala.concurrent.Await
import scala.concurrent.duration.Duration

trait App {
  def mainIO: IO[Unit]
  def rts: RTS = RTS.defaultRTS

  // TODO Feed arguments with the STM in IO? ... find something neat
  def main(args: Array[String]): Unit =
    Await.result(rts.unsafePerformIO(mainIO), Duration.Inf)
}
