package scato
package clazz

trait ApplyClass[F[_]] extends FunctorClass[F] { self =>
  final val apply: Apply[F] = new Apply[F] {
    def functor = self.functor
    def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = self.ap[A, B](fa)(f)
  }

  def ap[A, B](fa: F[A])(f: F[A => B]): F[B]
}
