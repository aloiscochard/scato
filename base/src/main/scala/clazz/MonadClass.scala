package scato
package clazz

trait MonadClass[F[_]] extends Monad[F] with BindClass[F] with ApplicativeClass[F] {
  override def map[A, B](ma: F[A])(f: (A) => B): F[B] = flatMap(ma)(a => pure(f(a)))

  implicit final def monad: Monad[F] = this
}
