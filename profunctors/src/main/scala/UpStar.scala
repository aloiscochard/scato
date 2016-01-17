package scato
package profunctors

import clazz.Functor

final case class UpStar[F[_], A, B](run: A => F[B])

object UpStar {
  implicit def profunctor[F[_]](implicit F: TC[F, Functor]): Profunctor[UpStar[F, ?, ?]] = new Profunctor[UpStar[F, ?, ?]] {
    override def lmap[A, B, C](fab: UpStar[F, A, B])(k: C => A): UpStar[F, C, B] =
      UpStar(a => fab.run(k(a)))
    override def rmap[A, B, C](fab: UpStar[F, A, B])(k: B => C): UpStar[F, A, C] =
      UpStar(a => F.instance.map(fab.run(a))(k))
    override def dimap[A, B, C, D](fab: UpStar[F, A, B])(ab: C => A)(cd: B => D): UpStar[F, C, D] =
      UpStar(a => F.instance.map(fab.run(ab(a)))(cd))
  }
}
