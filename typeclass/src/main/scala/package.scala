package scato

import Leibniz.===

trait ~~>[A[_[_]], B[_[_]]] {
  def apply[T[_]](at: A[T]): B[T]
}

trait ~~>>[A[_[_, _]], B[_[_, _]]] {
  def apply[T[_, _]](at: A[T]): B[T]
}

case class TC[T[_], C[_[_]]](instance: C[T]) extends AnyVal {
  def map[D[_[_]]](f: C ~~> D): TC[T, D] = TC(f(instance))
}

case class TC2[T[_, _], C[_[_, _]]](instance: C[T]) extends AnyVal {
  def map[D[_[_, _]]](f: C ~~>> D): TC2[T, D] = TC2(f(instance))
}

trait TCU[C[_[_]], TA] {
  type T[_]
  type A
  def instance: C[T]
  def leibniz: TA === T[A]

  def apply(ta: TA): T[A] = leibniz(ta)
  implicit def typeclass: TC[T, C] = TC[T, C](instance)
}

object TCU {
  implicit def tc[A0, T0[_], C[_[_]]](implicit TC0: TC[T0, C]): TCU[C, T0[A0]] {
    type T[X] = T0[X]
    type A = A0
  } = new TCU[C, T0[A0]] {
    type T[X] = T0[X]
    type A = A0
    val instance: C[T] = TC0.instance.asInstanceOf[C[T]]
    val leibniz: T0[A0] === T[A] = Leibniz.refl
  }
}

case class Unapply[TA](ta: TA) extends AnyVal {
  def instance[A, T[_], C[_[_]]](implicit TA: TCU[C, TA]): C[TA.T] = TA.instance
}
