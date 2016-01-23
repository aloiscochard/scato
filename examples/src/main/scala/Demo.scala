package scato
package examples

import data.Disjunction.{\/, right}
import clazz.Functor
import clazz.Functor.syntax._
import clazz.Traversable.syntax._

object Demo {
  def demo0(): Unit = {
    val xs: List[Int] = (0 to 128).toList
    val f: Int => String \/ Int = i => right(i)
    xs.traverse(f)
  }

  def demo1(): Unit = {

    def f[F[_]](fa: F[Int])(implicit F: Functor[F]): F[String] = fa.map(_.toString)
    val x: String \/ Int = right(42)

    x.map(_.toString)
  }
}

