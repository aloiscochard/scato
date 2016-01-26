package scato
package benchmarks

import scala.annotation.tailrec
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

import clazz.{Functor, FunctorClass}
import free.monad._

/*
@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class FreeMonads {

}

object FreeMonads {
  sealed trait Op[+A]

  object Op extends FunctorClass[Op] {
    case class Add(b: Int) extends Op[Nothing]
    case object Get extends Op[Nothing]

    def map[A, B](op: Op[A])(f: A => B): Op[B] = op match {
      case Add(i) => Add(i)
      case Get => Get
    }
  }

  def mkFree: Free[Op, Int] = {
    @tailrec def build(i: Int, prg: Free[Op, Unit]): Free[Op, Unit] =
      if (i < 1) prg else build(i - 1, prg.flatMap(_ => Free.liftF(Op.Add(i))))
    build(1000, Free.pure[Op, Unit](())).flatMap(_ => Free.liftF[Op, Int](Op.Get))
  }

  def run(free: Free[Op, Int]): Int = Free.run(free)(s
}
*/
