package scato

trait TCSyntax {
  implicit class RichUnapplyBase[TA](self: Unapply[TA]) {
    /*
    def functor[T[_]](implicit TA: TCU[T, Functor, TA]): Functor[T] = TA.instance
    def functorTC[T[_]](implicit TA: TCU[T, Functor, TA]): TC[T, Functor] = TA.TC
    def functorTCU[T[_]](implicit TA: TCU[T, Functor, TA]): TCU[T, Functor, TA] = TA
    */
  }
}
