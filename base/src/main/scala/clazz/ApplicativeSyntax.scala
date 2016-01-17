package scato
package clazz

import scala.language.implicitConversions

trait ApplicativeSyntax {
  implicit def applicativeOpsA[A](a: A): ApplicativeSyntax.OpsA[A] = new ApplicativeSyntax.OpsA(a)
}

object ApplicativeSyntax {
  class OpsA[A](a: A) {
    def pure[F[_]](implicit F: TC[F, Applicative]): F[A] = F.instance.pure(a)
  }
}
