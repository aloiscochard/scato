package scato
package transformers

import clazz.Applicative.syntax._
import clazz.Bind.syntax._
import clazz.Functor.syntax._
import clazz.Monad

import system.BindCore
import system.BindCore.Thunk
import system.Unsafe.Val

case class StateT[S, M[_], A] private[transformers](thunk: Thunk) {
  def ap[B](sab: StateT[S, M, A => B])(implicit M: Monad[M]): StateT[S, M, B] = flatMap(a => sab.map(_(a)))

  def flatMap[B](f: A => StateT[S, M, B])(implicit M: Monad[M]): StateT[S, M, B] =
    StateT(Thunk.map[M[(A, S)], M[(B, S)]](thunk)
      (mas => mas.flatMap { case (a, s) => StateT.run(f(a))(s) }))

 def map[B](f: A => B)(implicit M: Monad[M]): StateT[S, M, B] =
   StateT(Thunk.map[M[(A, S)], M[(B, S)]](thunk)
     (mas => mas.map { case (a, s) => (f(a), s) }))
}

object StateT {
  def pure[S, M[_], A](a: A)(implicit M: Monad[M]): StateT[S, M, A] =
    StateT(Thunk.map[S, M[(A, S)]](Nil)(s => (a, s).pure))

  def state[S, M[_], A](f: S => (A, S))(implicit M: Monad[M]): StateT[S, M, A] =
    StateT(Thunk.map[S, M[(A, S)]](Nil)(s => f(s).pure))

  def run[S, M[_], A](sma: StateT[S, M, A])(s: S): M[(A, S)] =
    BindCore.Thunk.eval[S, M[(A, S)]](sma.thunk, s)
}
