package scato
package clazz

abstract class Bind[M[_]] {
  def apply: Apply[M]
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
}

object Bind extends BindInstances {
  def apply[F[_]](implicit F: TC[F, Bind]): Bind[F] = F.instance

  object syntax extends BindSyntax
}
