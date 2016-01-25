package scato
package free
package monad

import clazz.{Applicative, Bind, Functor, Monad}

import data.Disjunction.\/
import data.Disjunction.{\/, L_, R_}

import system.BindCore
import system.BindCore.Thunk
import system.Unsafe.Val

case class FreeT[S[_], M[_], A] private[monad](thunk: Thunk) {
  import FreeT.{Core, Inter}

  def ap[B](sab: FreeT[S, M, A => B])(implicit S: Functor[S], M: Monad[M]): FreeT[S, M, B] = flatMap(a => sab.map(_(a)))

  def flatMap[B](f: A => FreeT[S, M, B])(implicit S: Functor[S], M: Monad[M]): FreeT[S, M, B] =
    FreeT(Thunk.map[Core[S, M, A], Core[S, M, B]](thunk)
      { case (i, mas) => (i, M.bind.flatMap(mas) {
          case L_(a) => M.bind.apply.functor.map(FreeT.run(f(a))(i.reify[S, M, B]))(L_(_))
          case R_(sma) => M.applicative.pure(R_(S.map(sma)(M.bind.flatMap(_)(a => FreeT.run(f(a))(i.reify[S, M, B])))))
      })})

  def map[B](f: A => B)(implicit S: Functor[S], M: Functor[M]): FreeT[S, M, B] =
    FreeT(Thunk.map[Core[S, M, A], Core[S, M, B]](thunk)
      { case (i, mas) => (i, M.map(mas) {
          case L_(a) => L_(f(a))
          case R_(sma) => R_(S.map(sma)(M.map(_)(f)))
      })})
}

object FreeT {
  class Inter(val value: Val) extends AnyVal { def reify[S[_], M[_], A]: S[M[A]] => M[A] = Val.reify[S[M[A]] => M[A]](value) }
  object Inter { def apply[S[_], M[_], A](f: S[M[A]] => M[A]) = new Inter(Val.cast(f)) }

  type Core[S[_], M[_], A] = (Inter, M[(A \/ S[M[A]])])

  def pure[S[_], M[_], A](a: A)(implicit M: Applicative[M]): FreeT[S, M, A] =
    FreeT(Thunk.map[Core[S, M, A], Core[S, M, A]](Nil) { case (i, _) => (i, M.pure(L_(a))) })

  def lift[S[_], M[_], A](ma: M[A])(implicit M: Functor[M]): FreeT[S, M, A] =
    FreeT(Thunk.map[Core[S, M, A], Core[S, M, A]](Nil) { case (i, _) => (i, M.map(ma)(L_(_))) })

  def liftF[S[_], M[_], A](sa: S[A])(implicit S: Functor[S], M: Applicative[M]): FreeT[S, M, A] =
    FreeT(Thunk.map[Core[S, M, A], Core[S, M, A]](Nil) { case (i, _) => (i, M.pure(R_(S.map(sa)(M.pure(_))))) })

  def run[S[_], M[_], A](sma: FreeT[S, M, A])(f: S[M[A]] => M[A])(implicit S: Functor[S], M: Monad[M]): M[A] = {
    val init = null.asInstanceOf[M[(A \/ S[M[A]])]]
    val (_, res) = BindCore.Thunk.eval[Core[S, M, A], Core[S, M, A]](sma.thunk, (Inter[S, M, A](f), init))
    M.bind.flatMap(res) {
      case L_(a) => M.applicative.pure(a)
      case R_(sma) => f(sma)
    }
  }
}

