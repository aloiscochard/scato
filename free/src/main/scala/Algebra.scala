package scato
package free

import System.Val

object Algebra {
  type Thunk = List[Op]

  object Thunk {
    import Op._
    import Val._

    def pure[A](f: Unit => A): Thunk =
      Map(_ => cast(f(()))) :: Nil

    def bind[A](xs: Thunk)(f: A => Thunk): Thunk =
      Bind(x => f(reify[A](x))) :: xs

    def map[A, B](xs: Thunk)(f: A => B): Thunk =
      Map(x => cast(f(reify[A](x)))) :: xs
  }

  sealed trait Op

  object Op {
    case class Map(f: Val => Val) extends Op
    case class Bind(f: Val => Thunk) extends Op
  }
}
