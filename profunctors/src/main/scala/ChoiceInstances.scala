package scato
package profunctors

import data.Disjunction.{\/, L_, R_}

trait ChoiceInstances {
  implicit val function: TC2[Function, Choice] = TC2(new Choice[Function] {
    val profunctor = Profunctor[Function]
    override def left[A, B, C](ab: A => B): A \/ C => B \/ C  = _.fold[B \/ C](a => L_(ab(a)))(R_(_))
    override def right[A, B, C](ab: A => B): C \/ A => C \/ B = _.fold[C \/ B](L_(_))((a => R_(ab(a))))
  })
}
