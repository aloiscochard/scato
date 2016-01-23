package scato
package clazz

import scala.language.implicitConversions

trait BindSyntax {
  def flatMap[M[_], A, B](ma: M[A])(f: A => M[B])(implicit M: Bind[M]): M[B] = M.flatMap(ma)(f)

  implicit def bindOps[M[_], A](ma: M[A])(implicit M: Bind[M]): BindSyntax.Ops[M, A] =
    new BindSyntax.Ops(ma)

  implicit def bindOpsU[M[_], MA](ma: MA)(implicit M: Unapply[Bind, MA]): BindSyntax.Ops[M.T, M.A] =
    new BindSyntax.Ops(M(ma))(M.instance)
}

object BindSyntax {
  class Ops[M[_], A](ma: M[A])(implicit M: Bind[M]) {
    def flatMap[B](f: A => M[B]): M[B] = M.flatMap(ma)(f)
  }
}

