package scato
package free
package monad

import Control.Lazy
import System.Val
import Algebra._

case class Free[F[_], A](thunk: Thunk) {
  def ap[B](fab: Free[F, A => B]): Free[F, B] = flatMap(a => fab.map(_(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] = Free(Thunk.bind(thunk)(a => f(a).thunk))
  def map[B](f: A => B): Free[F, B] = Free(Thunk.map(thunk)(f))
}

object Free extends FreeInstances {
  def run[F[_], A](free: Free[F, A]): F[A] = Val.reify[F[A]](Interpreter.eval(free.thunk))
}
