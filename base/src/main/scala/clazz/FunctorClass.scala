package scato
package clazz

trait FunctorClass[F[_]] { self =>
  final val functor: Functor[F] = new Functor[F] {
    def map[A, B](ma: F[A])(f: A => B): F[B] = self.map[A, B](ma)(f)
  }

  def map[A, B](ma: F[A])(f: A => B): F[B]
}
