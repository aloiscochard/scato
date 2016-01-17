package scato
package clazz

abstract class Apply[F[_]] {
  def functor: Functor[F]
  def ap[A, B](fa: F[A], f: F[A => B]): F[B]
}

object Apply {
  object syntax extends ApplySyntax
}
