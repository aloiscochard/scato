package scato

import Leibniz.===
import data.Disjunction.{\/}

trait TCUBase extends TCUBase.TCUBase0

object TCUBase {
  trait TCUBase0 {
    // Disjunction
    implicit def tcuT2[T0[_, _], C[_[_]], A0, B0](implicit
      TC0: TC[T0[B0, ?], C]
    ): TCU[C, T0[B0, A0]] {
      type T[X] = T0[B0, X]
      type A = A0
    } = new TCU[C, T0[B0, A0]] {
      override type T[X] = T0[B0, X]
      override type A = A0
      override val instance = TC0.instance.asInstanceOf[C[T]]
      override val instanceTag = TC0.instanceTag
      override val leibniz: T0[B0, A0] === T[A] = Leibniz.refl
    }

    // OptionT
    implicit def tcuT2a1[T0[_[_], _], C[_[_]], A0, F[_]](implicit
      TC0: TC[T0[F, ?], C]
    ): TCU[C, T0[F, A0]] {
      type T[X] = T0[F, X]
      type A = A0
    } = new TCU[C, T0[F, A0]] {
      override type T[X] = T0[F, X]
      override type A = A0
      override val instance = TC0.instance.asInstanceOf[C[T]]
      override val instanceTag = TC0.instanceTag
      override val leibniz: T0[F, A0] === T[A] = Leibniz.refl
    }
  }
}
