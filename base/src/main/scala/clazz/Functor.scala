package scato
package clazz

abstract class Functor[F[_]] {
  def map[A, B](ma: F[A])(f: A => B): F[B]
}

object Functor {
  object syntax extends FunctorSyntax {
    def map[F[_], A, B](fa: F[A])(f: A => B)(implicit F: TC[F, Functor]): F[B] = F.instance.map(fa)(f)
  }
}
