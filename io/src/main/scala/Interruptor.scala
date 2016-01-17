package scato
package io

trait Interruptor {
  def kill(): Unit
  def interrupted: Boolean
  def newChild: Interruptor
}

object Interruptor {
  abstract class UnInterruptor extends Interruptor {
    def kill() = {}
    def interrupted = false
  }

  val unintr = new UnInterruptor { def newChild: Interruptor = this }
}
