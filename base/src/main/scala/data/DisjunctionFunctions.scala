package scato
package data

trait DisjunctionFunctions {
  // TODO Can be inlined?
  def left[L](value: L): Disjunction[L, Nothing] = Disjunction.L_(value)
  def right[R](value: R): Disjunction[Nothing, R] = Disjunction.R_(value)
}
