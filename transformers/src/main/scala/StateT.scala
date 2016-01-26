package scato
package transformers

import clazz.{Functor, Monad}
import clazz.Applicative.syntax._
import clazz.Bind.syntax._
import clazz.Functor.syntax._

import system.BindCore
import system.BindCore.Thunk
import system.Unsafe.Val

case class StateT[S, M[_], A] private[transformers](thunk: Thunk) {
  def ap[B](sab: StateT[S, M, A => B])(implicit M: Monad[M]): StateT[S, M, B] = flatMap(a => sab.map(_(a)))

  def flatMap[B](f: A => StateT[S, M, B])(implicit M: Monad[M]): StateT[S, M, B] =
    StateT(Thunk.map[M[(A, S)], M[(B, S)]](thunk)
      (mas => mas.flatMap { case (a, s) => f(a).run(s) }))

  def map[B](f: A => B)(implicit M: Monad[M]): StateT[S, M, B] =
    StateT(Thunk.map[M[(A, S)], M[(B, S)]](thunk)
      (mas => mas.map { case (a, s) => (f(a), s) }))

  def run(s: S): M[(A, S)] =
    BindCore.Thunk.eval[S, M[(A, S)]](thunk, s)

  def exec(s: S)(implicit M: Functor[M]): M[S] =
    run(s).map { case (_, s) => s }
}

object StateT {
  class Syntax[S, M[_]](implicit M: Monad[M]) {
    def pure[A](a: A): StateT[S, M, A] = StateT.pure[S, M, A](a)
    def lift[A](ma: M[A]): StateT[S, M, A] = StateT.lift[S, M, A](ma)
    def modify(f: S => S): StateT[S, M, Unit] = StateT.modify[S, M](f)
  }

  def syntax[S, M[_]](implicit M: Monad[M]): Syntax[S, M] = new Syntax[S, M]

  def pure[S, M[_], A](a: A)(implicit M: Monad[M]): StateT[S, M, A] =
    StateT(Thunk.map[S, M[(A, S)]](Nil)(s => (a, s).pure))

  def state[S, M[_], A](f: S => (A, S))(implicit M: Monad[M]): StateT[S, M, A] =
    StateT(Thunk.map[S, M[(A, S)]](Nil)(s => f(s).pure))

  def lift[S, M[_], A](ma: M[A])(implicit M: Monad[M]): StateT[S, M, A] =
    StateT(Thunk.map[S, M[(A, S)]](Nil)(s => ma.map((_, s))))

  def modify[S, M[_]](f: S => S)(implicit M: Monad[M]): StateT[S, M, Unit] =
    StateT(Thunk.map[S, M[(Unit, S)]](Nil)(s => M.applicative.pure[(Unit, S)](((), f(s)))))
}
