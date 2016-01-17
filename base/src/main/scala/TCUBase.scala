package scato

import Leibniz.===
import data.Disjunction.{\/}

trait TCUBase extends TCUBase.TCUBase0

object TCUBase {
  trait TCUBase0 {
    implicit def tcuTC2A0[T0[_, _], C[_[_]], A0, B0](implicit
      TC0: TC[T0[B0, ?], C]
    ): TCU[C, T0[B0, A0]] {
      type T[X] = T0[B0, X]
      type A = A0
    } = new TCU[C, T0[B0, A0]] {
      type T[X] = T0[B0, X]
      type A = A0
      val instance: C[T] = TC0.instance.asInstanceOf[C[T]]
      val leibniz: T0[B0, A0] === T[A] = Leibniz.refl
    }

    implicit def tcuTC21A0[T0[_[_], _], C[_[_]], A0, F[_]](implicit
      TC0: TC[T0[F, ?], C]
    ): TCU[C, T0[F, A0]] {
      type T[X] = T0[F, X]
      type A = A0
    } = new TCU[C, T0[F, A0]] {
      type T[X] = T0[F, X]
      type A = A0
      val instance: C[T] = TC0.instance.asInstanceOf[C[T]]
      val leibniz: T0[F, A0] === T[A] = Leibniz.refl
    }
  }
}
