package scato
package clazz

import scala.language.implicitConversions

trait FunctorSyntax {
  implicit def functorOps[F[_], A](fa: F[A])(implicit F: TC[F, Functor]): FunctorSyntax.Ops[F, A] =
    new FunctorSyntax.Ops(fa)(F.instance)

  implicit def functorOpsU[F[_], FA](fa: FA)(implicit F: TCU[Functor, FA]): FunctorSyntax.Ops[F.T, F.A] =
    new FunctorSyntax.Ops(F(fa))(F.instance)
}

object FunctorSyntax {
  class Ops[F[_], A](fa: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map[A, B](fa)(f)
    def void: F[Unit] = F.map[A, Unit](fa)(_ => ())
  }
}


