package scato
package examples

import Prelude._

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
}

