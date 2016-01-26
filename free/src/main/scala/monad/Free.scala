package scato
package free
package monad

import clazz.Functor

object Free {
  def pure[F[_], A](a: A): FreeT[F, Identity, A] =
    FreeT.pure[F, Identity, A](a)

  def liftF[F[_], A](sa: F[A])(implicit F: Functor[F]): FreeT[F, Identity, A] =
    FreeT.liftF[F, Identity, A](sa)

  def run[F[_], A](free: FreeT[F, Identity, A])(f: F[A] => A)(implicit F: Functor[F]): A =
    FreeT.run[F, Identity, A](free)(sma => Identity(f(F.map(sma)(_.run)))).run
}
