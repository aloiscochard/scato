package scato
package clazz

import scala.annotation.tailrec

import Identity.Id
import data.Disjunction.{\/, L_, R_}

abstract class BindRec[M[_]] {
  def bind: Bind[M]
  def tailRecM[A, B](a: A)(f: A => M[A \/ B]): M[B]
}

object BindRec extends BindRecInstances
