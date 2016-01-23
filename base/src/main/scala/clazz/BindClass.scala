package scato
package clazz

trait BindClass[F[_]] extends Bind[F] with ApplyClass[F] {
  override def ap[A, B](fa: F[A])(f: F[A => B]): F[B] = flatMap(f)(map(fa))

  implicit final def bind: Bind[F] = this
}
