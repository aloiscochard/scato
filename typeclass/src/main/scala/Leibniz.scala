package scato

import Identity.Id

object Leibniz {
  trait ===[A, B] {
    def subst[F[_]](fa: F[A]): F[B]

    def apply(a: A): B = subst[Id](a)
    def onF[X](fa: X => A): X => B = subst[X => ?](fa)
  }

  /** Equality is reflexive -- we rely on subtyping to expand this type */
  def refl[A]: A === A = new (A === A) {
    def subst[F[_]](fa: F[A]): F[A] = fa
  }
}
