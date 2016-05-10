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
      (mas => mas.flatMap { case (a, s) => StateT.run[S, M, B](f(a))(s) }))

  def map[B](f: A => B)(implicit M: Monad[M]): StateT[S, M, B] =
    StateT(Thunk.map[M[(A, S)], M[(B, S)]](thunk)
      (mas => mas.map { case (a, s) => (f(a), s) }))
}

object StateT extends StateTInstances {
  class Syntax[S, M[_]](implicit M: Monad[M]) {
    def pure[A](a: A): StateT[S, M, A] = StateT.pure[S, M, A](a)
    def lift[A](ma: M[A]): StateT[S, M, A] = StateT.lift[S, M, A](ma)
    def modify(f: S => S): StateT[S, M, Unit] = StateT.modify[S, M](f)
  }

  def syntax[S, M[_]](implicit M: Monad[M]): Syntax[S, M] = new Syntax[S, M]

  def pure[S, M[_], A](a: A)(implicit M: Monad[M]): StateT[S, M, A] =
    StateT(Thunk.map[S, M[(A, S)]](Nil)(s => (a, s).pure[M]))

  def state[S, M[_], A](f: S => (A, S))(implicit M: Monad[M]): StateT[S, M, A] =
    StateT(Thunk.map[S, M[(A, S)]](Nil)(s => f(s).pure[M]))

  def lift[S, M[_], A](ma: M[A])(implicit M: Monad[M]): StateT[S, M, A] =
    StateT(Thunk.map[S, M[(A, S)]](Nil)(s => ma.map((_, s))))

  def modify[S, M[_]](f: S => S)(implicit M: Monad[M]): StateT[S, M, Unit] =
    StateT(Thunk.map[S, M[(Unit, S)]](Nil)(s => M.applicative.pure[(Unit, S)](((), f(s)))))

  def get[S, M[_]](implicit M: Monad[M]): StateT[S, M, S] =
    StateT(Thunk.map[S, M[(S, S)]](Nil)(s => (s, s).pure[M]))

  def run[S, M[_], A](sma: StateT[S, M, A])(s: S): M[(A, S)] =
    BindCore.Thunk.eval[S, M[(A, S)]](sma.thunk, s)

  def exec[S, M[_], A](sma: StateT[S, M, A])(s: S)(implicit M: Functor[M]): M[S] =
    run(sma)(s).map { case (_, s) => s }
}
