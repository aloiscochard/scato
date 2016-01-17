package scato
package clazz

abstract class Traversable[T[_]] {
  def functor: Functor[T]
  def foldable: Foldable[T]

  def traverse[F[_], A, B](ta: T[A])(f: A => F[B])(implicit F: TC[F, Applicative]): F[T[B]]
  def sequence[F[_], A](ta: T[F[A]])(implicit F: TC[F, Applicative]): F[T[A]]
}

object Traversable extends TraversableInstances {
  def apply[T[_]](implicit T: TC[T, Traversable]): Traversable[T] = T.instance
  implicit def traversable[T[_]](implicit T: Traversable[T]): TC[T, Traversable] = TC[T, Traversable](T)

  object syntax extends TraversableSyntax
}
