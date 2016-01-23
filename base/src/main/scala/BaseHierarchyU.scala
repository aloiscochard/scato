package scato

import Leibniz.===
import data.Disjunction.{\/}

trait BaseHierarchyU extends BaseHierarchyU.BHU0

object BaseHierarchyU {
  trait BHU0 {
    // Disjunction
    implicit def unapplyT2[T0[_, _], C[_[_]], A0, B0](implicit C: C[T0[B0, ?]]): Unapply[C, T0[B0, A0]] {
      type T[X] = T0[B0, X]
      type A = A0
    } = new Unapply[C, T0[B0, A0]] {
      override type T[X] = T0[B0, X]
      override type A = A0
      override def instance = C.asInstanceOf[C[T]]
      override def leibniz: T0[B0, A0] === T[A] = Leibniz.refl
    }

    // OptionT
    implicit def tcuT2a1[T0[_[_], _], C[_[_]], A0, F[_]](implicit C: C[T0[F, ?]]): Unapply[C, T0[F, A0]] {
      type T[X] = T0[F, X]
      type A = A0
    } = new Unapply[C, T0[F, A0]] {
      override type T[X] = T0[F, X]
      override type A = A0
      override def instance = C.asInstanceOf[C[T]]
      override def leibniz: T0[F, A0] === T[A] = Leibniz.refl
    }
  }
}
