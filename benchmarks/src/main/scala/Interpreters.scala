package scato
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

object Data {
  val xs: List[Int] = (1 to 100000).toList
}

object Scato {
  import Prelude._
  import transformers.State

  def mkState[F[_]](xs: F[Int])(f: Long => (Unit, Long))(implicit F: TC[F, Traversable]): State[Long, Unit] =
    xs.foldLeft(State.pure[Long, Unit](()))((s, _) => s.flatMap(_ => State.state(f)))

  def run[F[_]](xs: F[Int])(f: Long => (Unit, Long))(implicit F: TC[F, Traversable]): (Unit, Long) =
    State.run(mkState(xs)(f))(0)
}

object Scalaz {
  import scalaz._
  import scalaz.State
  import scalaz.Free._
  import scalaz.syntax.traverse._

  def mkState[F[_]](xs: F[Int])(f: Long => (Unit, Long))(implicit F: Traverse[F]) =
    xs.foldLeft(State.state[Long, Unit](()).liftF)((s, _) => s.flatMap(_ => State[Long, Unit](s => f(s).swap).liftF))

  def run[F[_]](xs: F[Int])(f: Long => (Unit, Long))(implicit F: Traverse[F]): (Unit, Long) =
    mkState(xs)(f).foldRun(0L)((a,b) => b(a)).swap
}

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class Interpreters {
  import Data._

  def f(i: Long): (Unit, Long) = ((), i + 1)

  @Benchmark def scalaz = {
    import _root_.scalaz.std.AllInstances._
    Scalaz.run(xs)(f _)
  }

  @Benchmark def scato  = Scato.run(xs)(f _)
}
