package scato
package clazz

import scala.annotation.tailrec

import data.Disjunction.{\/, L_, R_}

trait BindRecInstances {
  implicit val bindRecIdentity: TC[Identity, BindRec] = TC(new BindRec[Identity] {
    override val bind: Bind[Identity] = Monad[Identity].bind

    @tailrec
    override def tailRecM[A, B](a: A)(f: A => Identity[A \/ B]): Identity[B] =
      f(a) match {
        case Identity(L_(a0)) => tailRecM(a0)(f)
        case Identity(R_(b))  => Identity(b)
      }
  })
}
