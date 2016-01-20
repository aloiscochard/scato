package scato
package free

import scala.annotation.tailrec

import Algebra._
import System.Val

object Interpreter {
  def eval(thunk0: Thunk): Val = {
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

    go(thunk0.reverse, Val.unit, Nil)
  }
}
