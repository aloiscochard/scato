package scato
package free
package monad

import system.BindCore
import system.BindCore.Thunk

case class Free[F[_], A](thunk: Thunk) {
  def ap[B](fab: Free[F, A => B]): Free[F, B] =
    flatMap(a => fab.map(_(a)))
  def flatMap[B](f: A => Free[F, B]): Free[F, B] =
    Free(Thunk.bind[A, Free[F, B]](thunk)(a => f(a))(_.thunk))
  def map[B](f: A => B): Free[F, B] =
    Free(Thunk.map[A, B](thunk)(f))
}

object Free extends FreeInstances {
  def pure[F[_], A](a: A): Free[F, A] = Free(Thunk.pure[A](_ => a))
  def run[F[_], A](free: Free[F, A]): F[A] = BindCore.Thunk.eval_[F[A]](free.thunk)
}
