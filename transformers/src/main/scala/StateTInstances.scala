package scato
package transformers

import clazz.{Apply, Applicative, Bind, Functor, Monad}

trait StateTInstances {
  implicit def stateTBind[S, M[_]](implicit M: Monad[M]): Bind[StateT[S, M, ?]] =
    new Bind[StateT[S, M, ?]] {
      override val apply = new Apply[StateT[S, M, ?]] {
        override val functor = new Functor[StateT[S, M, ?]] {
          override def map[A, B](fa: StateT[S, M, A])(f: A => B): StateT[S, M, B] = fa.map(f)
        }
        override def ap[A, B](fa: StateT[S, M, A])(fab: StateT[S, M, A => B]): StateT[S, M, B] = fa.ap(fab)
      }

      override def flatMap[A, B](fa: StateT[S, M, A])(fb: A => StateT[S, M, B]): StateT[S, M, B] = fa.flatMap(fb)
    }

  implicit def stateTMonad[S, M[_]](implicit M: Monad[M]): Monad[StateT[S, M, ?]] =
    new Monad[StateT[S, M, ?]] {
      override val bind = stateTBind[S, M]
      override val applicative = new Applicative[StateT[S, M, ?]] {
        override val apply = stateTBind[S, M].apply
        override def pure[A](a: A) = StateT.pure[S, M, A](a)
      }
    }
}
