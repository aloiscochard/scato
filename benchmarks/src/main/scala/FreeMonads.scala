package scato
package benchmarks

import scala.annotation.tailrec
import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class FreeMonads {
  def runScato(size: Int) = {
    import transformers.State
    import FreeMonadsScato._
    run(mkFreeT[State[Int, ?]](size))
  }

  def runScalaz(size: Int): Int =  {
    import FreeMonadsScalaz._
    run(mkFree(size))
  }

  @Benchmark def free128Scato = runScato(128)
  @Benchmark def free128Scalaz = runScalaz(128)

  @Benchmark def free1024Scato = runScato(1024)
  @Benchmark def free1024Scalaz = runScalaz(1024)

  @Benchmark def free10000Scato = runScato(10000)
  @Benchmark def free10000Scalaz = runScalaz(10000)
}

object FreeMonadsScato {
  import clazz.{Functor, FunctorClass, Monad}
  import free.monad._
  import transformers.State

  case class Op[A](a: A)

  object Op extends FunctorClass[Op] {
    def map[A, B](op: Op[A])(f: A => B): Op[B] = op match {
      case Op(a) => Op(f(a))
    }
  }

  def mkFreeT[M[_]: Monad](size: Int): FreeT[Op, M, Unit] = {
    @tailrec def build(i: Int, prg: FreeT[Op, M, Unit]): FreeT[Op, M, Unit] =
      if (i < 1) prg else build(i - 1, prg.flatMap(_ => FreeT.liftF(Op(()))))
    build(size, FreeT.pure[Op, M, Unit](()))
  }

  def run(prg: FreeT[Op, State[Int, ?], Unit]): Int = State.exec(FreeT.run[Op, State[Int, ?], Unit](prg) {
    case Op(a) => State.modify[Int](_ + 1)
  })(0)
}

object FreeMonadsScalaz {
  import scalaz.{~>, Free, Functor, IndexedStateT, State, StateT, Monad}
  import Free.Trampoline

  case class Op[A](a: A)

  object Op {
    implicit val functor: Functor[Op] = new Functor[Op] {
      def map[A, B](op: Op[A])(f: A => B): Op[B] = Op(f(op.a))
    }
  }

  val opToStateT: Op ~> StateT[Trampoline, Int, ?] = new (Op ~> StateT[Trampoline, Int, ?]) {
    override def apply[A](oa: Op[A]): StateT[Trampoline, Int, A] = oa match {
      case Op(a) => State.modify[Int](_ + 1).lift[Trampoline].flatMap(_ => State.state(a).lift[Trampoline])
    }
  }

  def mkFree(size: Int): Free[Op, Unit] = {
    @tailrec def build(i: Int, prg: Free[Op, Unit]): Free[Op, Unit] =
      if (i < 1) prg else build(i - 1, prg.flatMap(_ => Free.liftF(Op(()))))
    build(size, Free.point[Op, Unit](()))
  }

  def run(prg: Free[Op, Unit]): Int =
    prg.foldMap[StateT[Trampoline, Int, ?]](opToStateT)(StateT.stateTMonadState[Int, Trampoline]).exec(0).run
}
