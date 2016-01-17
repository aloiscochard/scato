package scato
package data

sealed trait Disjunction[+L, +R] {
  import Disjunction.{L_, R_}
  final def fold[A](la: L => A)(ra: R => A): A = this match {
    case L_(l) => la(l)
    case R_(r) => ra(r)
  }
}

object Disjunction extends DisjunctionInstances {
  object Syntax extends DisjunctionSyntax

  type \/[L, R] = Disjunction[L, R]
  case class L_[L](value: L) extends (L \/ Nothing)
  case class R_[R](value: R) extends (Nothing \/ R)

  def swap[L, R](ab: L \/ R): R \/ L = ab.fold[R \/ L](R_(_))(L_(_))

  def fromEither[L, R](ab: Either[L, R]): L \/ R = ab.fold(L_(_), R_(_))
}
