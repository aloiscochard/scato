package scato
package clazz

abstract class Bind[M[_]] {
  def apply: Apply[M]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
}

object Bind extends BindInstances {
  def apply[F[_]](implicit F: TC[F, Bind]): Bind[F] = F.instance
  implicit def bind[M[_]](implicit M: Bind[M]): TC[M, Bind] = TC[M, Bind](M)

  object syntax extends BindSyntax {
    def flatMap[M[_], A, B](ma: M[A])(f: A => M[B])(implicit M: TC[M, Bind]): M[B] = M.instance.flatMap(ma)(f)
  }
}
