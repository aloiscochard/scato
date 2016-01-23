package scato
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

object Scato {
  import Prelude._
  import transformers.State

  def mkState[F[_]](xs: F[Int])(f: Long => (Unit, Long))(implicit F: Traversable[F]): State[Long, Unit] =
    xs.foldLeft(State.pure[Long, Unit](()))((s, _) => s.flatMap(_ => State.state(f)))

  def run[F[_]](xs: F[Int])(f: Long => (Unit, Long))(implicit F: Traversable[F]): (Unit, Long) =
    State.run(mkState(xs)(f))(0)
}

object Scalaz {
  import scalaz._
  import scalaz.State
  import scalaz.Free._
  import scalaz.syntax.traverse._

  def mkState[F[_]](xs: F[Int])(f: Long => (Long, Unit))(implicit F: Traverse[F]) =
    xs.foldLeft(State.state[Long, Unit](()).liftF)((s, _) => s.flatMap(_ => State[Long, Unit](s => f(s)).liftF))

  def run[F[_]](xs: F[Int])(f: Long => (Long, Unit))(implicit F: Traverse[F]): (Long, Unit) =
    mkState(xs)(f).foldRun(0L)((a,b) => b(a))
}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class Interpreters {
  import Data._

  def f0(i: Long): (Unit, Long) = ((), i + 1)
  def f1(i: Long): (Long, Unit) = (i + 1, ())

  @Benchmark def scalaz = {
    import _root_.scalaz.std.AllInstances._
    Scalaz.run(xs)(f1 _)
  }

  @Benchmark def scato  = Scato.run(xs)(f0 _)
}
