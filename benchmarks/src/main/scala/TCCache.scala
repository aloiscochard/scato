package scato
package benchmarks

import scala.reflect.runtime.universe.TypeTag
import scala.collection.concurrent.TrieMap

// A cache for TC instances, which have not proved it's used yet.
object TCCache {

  private val cache: TrieMap[String, Any] = TrieMap()

  type TypeTag[ID] = scala.reflect.runtime.universe.TypeTag[ID]

  def capture[T[_], C[_[_]], ID](i: => C[T])(implicit ID: TypeTag[ID]): TC[T, C] =
    new TC[T, C] {
      override def instance = cache.getOrElseUpdate(ID.toString, i).asInstanceOf[C[T]]
    }
}



