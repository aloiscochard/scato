package scato
package clazz

abstract class Apply[F[_]] {
  def functor: Functor[F]
  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}

object Apply {
  def apply[F[_]](implicit F: TC[F, Apply]): Apply[F] = F.instance
  implicit def apply_[M[_]](implicit M: Apply[M]): TC[M, Apply] = TC[M, Apply](M)

  object syntax extends ApplySyntax
}
