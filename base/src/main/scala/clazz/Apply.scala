package scato
package clazz

abstract class Apply[F[_]] {
  def functor: Functor[F]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}

object Apply {
  def apply[F[_]](implicit F: TC[F, Apply]): Apply[F] = F.instance

  object syntax extends ApplySyntax
}
