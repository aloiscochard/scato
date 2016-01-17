package scato
package clazz

import scala.language.implicitConversions

trait TraversableSyntax {
  // TODO Try to implement with Unapply.
  def sequence[T[_], F[_], A](tfa: T[F[A]])(implicit F: TC[F, Applicative], T: TC[T, Traversable]): F[T[A]] =
    T.instance.sequence(tfa)

  implicit def traversableOps[T[_], A](ta: T[A])(implicit T: TC[T, Traversable]): TraversableSyntax.Ops[T, A] =
    new TraversableSyntax.Ops(ta)(T.instance)

  implicit def traversable[T[_], TA](ta: TA)(implicit T: TCU[Traversable, TA]): TraversableSyntax.Ops[T.T, T.A] =
    new TraversableSyntax.Ops(T(ta))(T.instance)
}

object TraversableSyntax {
  class Ops[T[_], A](self: T[A])(implicit T: Traversable[T]) {
    def traverse[FB](f: A => FB)(implicit FB: TCU[Applicative, FB]): FB.T[T[FB.A]] =
      T.traverse(self)(FB.leibniz.onF(f))(FB.typeclass)
  }
}
