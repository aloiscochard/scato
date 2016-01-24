package scato
package transformers

import clazz.Monad

trait MonadTrans[T[_[_], _]] {
  def lift[M[_]: Monad, A](ma: M[A]): T[M, A]
}
