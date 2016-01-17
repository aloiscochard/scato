package scato
package profunctors

import scala.language.implicitConversions

trait ProfunctorSyntax {
  implicit def profunctorOps[F[_, _], A, B](fa: F[A, B])(implicit F: TC2[F, Profunctor]): ProfunctorSyntax.Ops[F, A, B] =
    new ProfunctorSyntax.Ops(fa)(F.instance)

    /*
  implicit def profunctorOpsU[F[_, _], FA](fa: FA)(implicit F: TCU[Functor, FA]): FunctorSyntax.Ops[F.T, F.A] =
    new FunctorSyntax.Ops(F(fa))(F.instance)
    */
}

object ProfunctorSyntax {
  class Ops[F[_, _], A, B](self: F[A, B])(implicit F: Profunctor[F]) {
    def lmap[C](ac: C => A): F[C, B] = F.lmap(self)(ac)
    def rmap[C](bc: B => C): F[A, C] = F.rmap(self)(bc)
    def dimap[C, D](ab: C => A)(bc: B => D): F[C, D] = F.dimap[A, B, C, D](self)(ab)(bc)
  }
}


