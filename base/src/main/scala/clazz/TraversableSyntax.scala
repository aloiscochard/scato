package scato
package clazz

import scala.language.implicitConversions

trait TraversableSyntax {
  // TODO Try to implement with Unapply.
  def sequence[T[_], F[_], A](tfa: T[F[A]])(implicit F: Applicative[F], T: Traversable[T]): F[T[A]] =
    T.sequence(tfa)

  implicit def traversableOps[T[_], A](ta: T[A])(implicit T: Traversable[T]): TraversableSyntax.Ops[T, A] =
    new TraversableSyntax.Ops(ta)

  implicit def traversable[T[_], TA](ta: TA)(implicit T: Unapply[Traversable, TA]): TraversableSyntax.Ops[T.T, T.A] =
    new TraversableSyntax.Ops(T(ta))(T.instance)
}

object TraversableSyntax {
  class Ops[T[_], A](self: T[A])(implicit T: Traversable[T]) {
    def traverse[FB](f: A => FB)(implicit FB: Unapply[Applicative, FB]): FB.T[T[FB.A]] =
      T.traverse(self)(FB.leibniz.onF(f))(FB.instance)
  }
}
