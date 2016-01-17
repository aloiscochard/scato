package scato
package clazz

abstract class Foldable[F[_]] {
  // def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: TC[B, Monoid]): B
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B
  def toList[A](fa: F[A]): List[A] = foldLeft(fa, List[A]())((t, h) => h :: t).reverse
}

object Foldable extends FoldableInstances {
  def apply[F[_]](implicit F: TC[F, Foldable]): Foldable[F] = F.instance
  implicit def foldable[F[_]](implicit F: Foldable[F]): TC[F, Foldable] = TC[F, Foldable](F)

  object syntax extends FoldableSyntax
}
