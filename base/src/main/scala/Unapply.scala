package scato

import Leibniz.===

trait Unapply[C[_[_]], TA] {
  type T[_]
  type A
  def instance: C[T]
  def leibniz: TA === T[A]

  def apply(ta: TA): T[A] = leibniz(ta)
}

object Unapply {
  implicit def tc[A0, T0[_], C[_[_]]](implicit C: C[T0]): Unapply[C, T0[A0]] {
    type T[X] = T0[X]
    type A = A0
  } = new Unapply[C, T0[A0]] {
    override type T[X] = T0[X]
    override type A = A0
    override def instance = C
    override def leibniz: T0[A0] === T[A] = Leibniz.refl
  }
}
