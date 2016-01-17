package scato
package clazz

trait ApplicativeSyntax {
  class Pure[F[_]](implicit F: TC[F, Applicative]) {
    def apply[A](a: A): F[A] = F.instance.pure(a)
  }

  def pure[F[_]](implicit F: TC[F, Applicative]): Pure[F] = new Pure[F]
}
