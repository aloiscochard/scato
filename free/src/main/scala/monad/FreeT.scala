package scato
package free
package monad

import clazz.{Applicative, Bind, Functor, Monad}

import data.Disjunction.\/
import data.Disjunction.{\/, L_, R_}

import system.BindCore
import system.BindCore.Thunk
import system.Unsafe.Val

case class FreeT[F[_], M[_], A] private[monad](thunk: Thunk) {
  import FreeT.{Core, Inter}

  def ap[B](fab: FreeT[F, M, A => B])(implicit F: Functor[F], M: Monad[M]): FreeT[F, M, B] = flatMap(a => fab.map(_(a)))

  def flatMap[B](f: A => FreeT[F, M, B])(implicit F: Functor[F], M: Monad[M]): FreeT[F, M, B] =
    FreeT(Thunk.map[Core[F, M, A], Core[F, M, B]](thunk)
      { case (i, mas) => (i, M.bind.flatMap(mas) {
          case L_(a) => M.bind.apply.functor.map(f(a).run(i.reify[F, M, B]))(L_(_))
          case R_(fma) => M.applicative.pure(R_(F.map(fma)(M.bind.flatMap(_)(a => f(a).run(i.reify[F, M, B])))))
      })})

  def map[B](f: A => B)(implicit F: Functor[F], M: Functor[M]): FreeT[F, M, B] =
    FreeT(Thunk.map[Core[F, M, A], Core[F, M, B]](thunk)
      { case (i, mas) => (i, M.map(mas) {
          case L_(a) => L_(f(a))
          case R_(fma) => R_(F.map(fma)(M.map(_)(f)))
      })})


  // def foldFreeT :: (Traversable f, Monad m) => (a -> m b) -> (f b -> m b) -> FreeT f m a -> m b
  //
  // def fold[A, B](f: A => M[B])(g: F[B] => M[B])(implicit F: Functor[F], M: Monad[M]): M[A] =

  def run(f: F[M[A]] => M[A])(implicit M: Monad[M]): M[A] = {
    val init = null.asInstanceOf[M[(A \/ F[M[A]])]]
    val (_, res) = BindCore.Thunk.eval[Core[F, M, A], Core[F, M, A]](thunk, (Inter[F, M, A](f), init))
    M.bind.flatMap(res) {
      case L_(a) => M.applicative.pure(a)
      case R_(fma) => f(fma)
    }
  }
}

object FreeT {
  class Inter(val value: Val) extends AnyVal { def reify[F[_], M[_], A]: F[M[A]] => M[A] = Val.reify[F[M[A]] => M[A]](value) }
  object Inter { def apply[F[_], M[_], A](f: F[M[A]] => M[A]) = new Inter(Val.cast(f)) }

  type Core[F[_], M[_], A] = (Inter, M[(A \/ F[M[A]])])

  def pure[F[_], M[_], A](a: A)(implicit M: Applicative[M]): FreeT[F, M, A] =
    FreeT(Thunk.map[Core[F, M, A], Core[F, M, A]](Nil) { case (i, _) => (i, M.pure(L_(a))) })

  def lift[F[_], M[_], A](ma: M[A])(implicit M: Functor[M]): FreeT[F, M, A] =
    FreeT(Thunk.map[Core[F, M, A], Core[F, M, A]](Nil) { case (i, _) => (i, M.map(ma)(L_(_))) })

  def liftF[F[_], M[_], A](fa: F[A])(implicit F: Functor[F], M: Applicative[M]): FreeT[F, M, A] =
    FreeT(Thunk.map[Core[F, M, A], Core[F, M, A]](Nil) { case (i, _) => (i, M.pure(R_(F.map(fa)(M.pure(_))))) })
}

