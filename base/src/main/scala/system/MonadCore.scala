package scato
package system

import scala.annotation.tailrec
import Unsafe.Val

object MonadCore {
  type Thunk = List[Op]

  object Thunk {
    import Op._
    import Val._

    def pure[A](f: Unit => A): Thunk =
      Map(_ => cast(f(()))) :: Nil

    def ap[A, B](xs: Thunk)(fab: Thunk): Thunk =
      Ap(fab) :: xs

    def map[A, B](xs: Thunk)(f: A => B): Thunk =
      Map(x => cast(f(reify[A](x)))) :: xs

    def bind[A, B](xs: Thunk)(f: A => B)(thunk: B => Thunk): Thunk =
      Bind(x => thunk(f(reify[A](x)))) :: xs
  }

  sealed trait Op

  object Op {
    case class Ap(fab: Thunk) extends Op
    case class Map(f: Val => Val) extends Op
    case class Bind(f: Val => Thunk) extends Op
  }

  def eval[A, B](thunk: Thunk, a: A): B = Val.reify[B](unsafeEval(thunk, Val.cast[A](a)))
  def eval_[A](thunk: Thunk): A = Val.reify[A](unsafeEval_(thunk))

  def unsafeEval(thunk0: Thunk, value0: Val): Val = {
    @tailrec
    def go(thunk: List[Op], value: Val, stack: List[List[Op]]): Val =
      thunk.headOption match {
        case None => stack.headOption  match {
          case None => value
          case Some(thunk0) =>
            val f = Val.reify[Val => Val](value)
            go(thunk, f(value), stack.tail)
        }
        case Some(op) =>
          op match {
            case Op.Ap(fab) => go(fab.reverse, value, thunk::stack)
            case Op.Map(f) => go(thunk.tail, f(value), stack)
            case Op.Bind(f) => go(f(value).reverse, Val.unit, stack)
          }
      }

    go(thunk0.reverse, value0, Nil)
  }
  def unsafeEval_(thunk: Thunk): Val = unsafeEval(thunk, Val.unit)
}
