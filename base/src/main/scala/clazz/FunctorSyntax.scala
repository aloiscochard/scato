package scato
package clazz

import scala.language.implicitConversions

trait FunctorSyntax {
  def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: TC[F, Functor]): F[B] = F.instance.map(fa)(f)

  implicit def functorOps[F[_], A](fa: F[A])(implicit F: TC[F, Functor]): FunctorSyntax.Ops[F, A] =
    new FunctorSyntax.Ops(fa)(F.instance)

  implicit def functorOpsU[F[_], FA](fa: FA)(implicit F: TCU[Functor, FA]): FunctorSyntax.Ops[F.T, F.A] =
    new FunctorSyntax.Ops(F(fa))(F.instance)
}

object FunctorSyntax {
  class Ops[F[_], A](self: F[A])(implicit F: Functor[F]) {
    def map[B](f: A => B): F[B] = F.map[A, B](self)(f)
    def void: F[Unit] = F.map[A, Unit](self)(_ => ())
  }
}


