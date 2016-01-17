package scato
package profunctors

import clazz.Functor

final case class DownStar[F[_], A, B](run: F[A] => B)

object DowStar {
  def profunctor[F[_]](implicit F: TC[F, Functor]): Profunctor[DownStar[F, ?, ?]] = new Profunctor[DownStar[F, ?, ?]] {
    override def lmap[A, B, C](fbc: DownStar[F, A, B])(k: C => A): DownStar[F, C, B] =
      DownStar(fb => fbc.run(F.instance.map(fb)(k)))
    override def rmap[A, B, C](fab: DownStar[F, A, B])(k: B => C): DownStar[F, A, C] =
      DownStar(fa => k(fab.run(fa)))
    override def dimap[A, B, C, D](fbc: DownStar[F, A, B])(ab: C => A)(cd: B => D): DownStar[F, C, D] =
      DownStar(fb => cd(fbc.run(F.instance.map(fb)(ab))))
  }
}
