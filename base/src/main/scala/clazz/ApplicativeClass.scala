package scato
package clazz


trait ApplicativeClass[F[_]] extends Applicative[F] with ApplyClass[F] {
  def map[A, B](ma: F[A])(f: (A) => B): F[B] = ap(ma)(pure(f))

  implicit final def applicative: Applicative[F] = this
}
