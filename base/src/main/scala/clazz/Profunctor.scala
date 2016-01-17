package scato
package clazz

abstract class Profunctor[F[_, _]] {
  def lmap[A, B, C](ab: A => B): F[B, C] => F[A, C] = dimap[A, B, C, C](ab)(identity)
  def rmap[A, B, C](bc: B => C): F[A, B] => F[A, C] = dimap[A, A, B, C](identity)(bc)
  def dimap[A, B, C, D](ab: A => B)(bc: C => D): F[B, C] => F[A, D] = lmap(ab).andThen(rmap(bc))
}
