package scato
package free
package monad

import clazz.Functor

object Free {
  def pure[S[_], A](a: A): FreeT[S, Identity, A] =
    FreeT.pure[S, Identity, A](a)

  def liftF[S[_], A](sa: S[A])(implicit S: Functor[S]): FreeT[S, Identity, A] =
    FreeT.liftF[S, Identity, A](sa)

  def run[S[_], A](free: FreeT[S, Identity, A])(f: S[A] => A)(implicit S: Functor[S]): A =
    FreeT.run[S, Identity, A](free)(sma => Identity(f(S.map(sma)(_.run)))).run
}
