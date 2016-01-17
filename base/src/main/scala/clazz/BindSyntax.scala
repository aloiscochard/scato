package scato
package clazz

import scala.language.implicitConversions

trait BindSyntax {
  implicit def bindOps[M[_], A](ma: M[A])(implicit M: TC[M, Bind]): BindSyntax.Ops[M, A] =
    new BindSyntax.Ops(ma)(M.instance)

  implicit def bindOpsU[M[_], MA](ma: MA)(implicit M: TCU[Bind, MA]): BindSyntax.Ops[M.T, M.A] =
    new BindSyntax.Ops(M(ma))(M.instance)
}

object BindSyntax {
  class Ops[M[_], A](ma: M[A])(implicit M: Bind[M]) {
    def flatMap[B](f: A => M[B]): M[B] = M.flatMap(ma)(f)
  }
}

