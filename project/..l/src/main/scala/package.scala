package scato {
  trait ~~>[A[_[_]], B[_[_]]] { def apply[T[_]](at: A[T]): B[T] }
  trait ~~>>[A[_[_, _]], B[_[_, _]]] { def apply[T[_, _]](at: A[T]): B[T] }

  case class TC[T[_], C[_[_]]](instance: C[T]) extends AnyVal {
    def map[D[_[_]]](f: C ~~> D): TC[T, D] = TC(f(instance))
  }

  trait TCU[T[_], C[_[_]], TA] {
    def TC: TC[T, C]
    implicit def instance: C[T] = TC.instance
  }

  case class Unapply[TA](ta: TA) {
    def instance[T[_], C[_[_]]](implicit TA: TCU[T, C, TA]): C[T] = TA.instance
  }
}

    // FunctorU { def functor[T[_]](implicit TA: TCU[T, Functor, TA]): Functor[T] = TA.instance }
