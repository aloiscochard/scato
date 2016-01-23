package scato
package clazz

trait ApplicativeClass[F[_]] extends ApplyClass[F] { self =>
  final val applicative: Applicative[F] = new Applicative[F] {
    def apply = self.apply
    def pure[A] (a: A): F[A] = self.pure[A](a)
  }

  def pure[A](a: A): F[A]
}
