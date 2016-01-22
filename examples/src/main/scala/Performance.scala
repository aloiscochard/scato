package scato
package examples

import Prelude._
import transformers.State.state
import transformers.StateT

object Caching {

  def doIt(): Unit = {
    val xs: List[Int] = (0 to 128).toList
    val f: Int => String \/ Int = i => right(i)
    xs.traverse(f)
  }

  def doItMoreTimes(): Unit = {
    (0 to 128).foreach { _ =>
      doIt()
    }
  }

  def doItWithState(): Unit = {
    val xs: List[Int] = (0 to 100000).toList

    def f[F[_]](s: F[Int])(implicit F: TC[F, Traversable]) = {
      s.map(ii => state[Int, Int](i => (i,ii)))
        .foldLeft(state[Int, Int](i => (i,0)))((s,a) => s.flatMap(i => a.map(ii => i )))
    }

    println(StateT.run(f(xs))(0))

  }
}

