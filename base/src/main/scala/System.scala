package scato

object System {

  type Val = Any with ({ type Tag = Any })

  object Val {
    val unit: Val = cast(Unit)
    def cast[A](x: Any): Val = x.asInstanceOf[Val]
    def castF[F[_], A](x: F[A]): F[Val] = x.asInstanceOf[F[Val]]
    def reify[A](x: Val): A = x.asInstanceOf[A]
    def reifyF[F[_], A](x: F[Val]): F[A] = x.asInstanceOf[F[A]]
  }
}
