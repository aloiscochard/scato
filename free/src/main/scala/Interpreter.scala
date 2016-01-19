package scato
package free

import scala.annotation.tailrec

import Algebra._
import System.Val

object Interpreter {
  def eval(thunk0: Thunk): Val = {
    @tailrec
    def go(thunk: List[Op], value: Val): Val =
      thunk.headOption match {
        case None => value
        case Some(op) =>
          op match {
            case Op.Map(f) => go(thunk.tail, f(value))
            case Op.Bind(f) => go(f(value).reverse, Val.unit)
          }
      }

    go(thunk0.reverse, Val.unit)
  }
}
