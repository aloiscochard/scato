package scato
package clazz

trait Foldable[F[_]] {
  // def foldMap[A,B](fa: F[A])(f: A => B)(implicit F: TC[B, Monoid]): B
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B
  def toList[A](fa: F[A]): List[A] = foldLeft(fa, List[A]())((t, h) => h :: t).reverse
}

object Foldable extends FoldableInstances {
  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F

  object syntax extends FoldableSyntax
}
