package scato
package examples

import Prelude._

object Demo {
  def demo0(): Unit = {
    val xs: List[Int] = (0 to 128).toList
    val f: Int => String \/ Int = i => right(i)
    xs.traverse(f)
  }
}

