package scato
package clazz

import scala.language.implicitConversions

trait ApplySyntax {
  implicit def applyOps[F[_], A](fa: F[A])(implicit F: TC[F, Apply]): ApplySyntax.Ops[F, A] =
    new ApplySyntax.Ops(fa)(F.instance)

  implicit def applyOpsU[F[_], FA](fa: FA)(implicit F: TCU[Apply, FA]): ApplySyntax.Ops[F.T, F.A] =
    new ApplySyntax.Ops[F.T, F.A](F(fa))(F.instance)
}

object ApplySyntax {
  class Ops[F[_], A](fa: F[A])(implicit F: Apply[F]) {
    def apply[B](fab: F[A => B]): F[B] = F.ap(fa)(fab)

    def <*>[B](fb: F[B]): F[(A, B)] =
      F.ap[A, (A, B)](fa)(F.functor.map[B, A => (A, B)](fb)(b => a => (a, b)))

    def *>[B](fb: F[B]): F[B] =
      F.functor.map[(A, B), B](<*>(fb)){ case (_, b) => b }
  }
}
