package scato
package data

import Disjunction.{\/, L_, R_}

trait DisjunctionSyntax {
  implicit class ToEitherOps[A](a: A) {
    def left[B]: A \/ B = L_(a)
    def right[B]: B \/ A = R_(a)
  }

  implicit class EitherAsDisjunction[A, B](ab: Either[A, B]) {
    def asDisjunction: A \/ B = Disjunction.fromEither(ab)
  }
}
