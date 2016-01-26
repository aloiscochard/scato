package scato
package benchmarks

import scala.annotation.tailrec
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

import clazz.{Functor, FunctorClass, Monad}
import free.monad._
import transformers.State

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class FreeMonads {

  @Benchmark def freeT = {
    import FreeMonads._
    run(mkFreeT[State[Int, ?]])
  }
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

  def mkFreeT[M[_]: Monad]: FreeT[Op, M, Int] = {
    @tailrec def build(i: Int, prg: FreeT[Op, M, Unit]): FreeT[Op, M, Unit] =
      if (i < 1) prg else build(i - 1, prg.flatMap(_ => FreeT.liftF(Op.Add(i))))
    build(1000, FreeT.pure[Op, M, Unit](())).flatMap(_ => FreeT.liftF[Op, M, Int](Op.Get))
  }

  def run(freeT: FreeT[Op, State[Int, ?],Int]): Int = State.exec(freeT.run {
    case Op.Add(i) => State.modify[Int](_ + 1).flatMap(_ => State.pure(i))
    case Op.Get => State.get[Int]
  })(0)
}
