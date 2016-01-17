package scato
package clazz

abstract class Bind[M[_]] {
  def apply: Apply[M]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
}

object Bind extends BindInstances {
  object syntax extends BindSyntax {
    def flatMap[M[_], A, B](ma: M[A])(f: A => M[B])(implicit M: TC[M, Bind]): M[B] = M.instance.flatMap(ma)(f)
  }
}
