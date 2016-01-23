package scato
package clazz

trait ApplicativeClass[F[_]] extends ApplyClass[F] { self =>
  final implicit val applicative: Applicative[F] = new Applicative[F] {
    def apply = self.apply
    def pure[A] (a: A): F[A] = self.pure[A](a)
  }
  override def map[A, B](fa: F[A])(f: A => B): F[B] = ap(fa)(pure(f))
  def pure[A](a: A): F[A]
}
