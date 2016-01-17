package scato
package clazz

import data.Disjunction.{\/}

abstract class MonadRec[M[_]] {
  def monad: Monad[M]
  def tailRecM[A, B](a: A)(f: A => M[A \/ B]): M[B]
}

object MonadRec
