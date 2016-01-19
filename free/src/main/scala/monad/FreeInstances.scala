package scato
package free
package monad

import clazz.{Apply, Applicative, Bind, BindRec, Functor, Monad}

trait FreeInstances {
  implicit def freeBind[F[_]](implicit F: TC[F, Functor]): Bind[Free[F, ?]] = new Bind[Free[F, ?]] {
    override val apply = new Apply[Free[F, ?]] {
      override val functor = new Functor[Free[F, ?]] {
        override def map[A, B](fa: Free[F, A])(f: A => B): Free[F, B] = fa.map(f)
      }
      override def ap[A, B](fa: Free[F, A])(fab: Free[F, A => B]): Free[F, B] = fa.ap(fab)
    }

    override def flatMap[A, B](fa: Free[F, A])(fb: A => Free[F, B]): Free[F, B] = fa.flatMap(fb)
  }

  // TODO Implement bindRec instance
  /*
  implicit def freeBindRec[F[_]](implicit F: TC[F, Functor]): BindRec[Free[F, ?]] = new BindRec[Free[F, ?]] {
    override val bind = freeBind[F]
    def tailRecM[A, B](a: A)(f: A => Free[F, A \/ B]): Free[F, B] =
      ???
  }
  */

  implicit def freeMonad[F[_]](implicit F: TC[F, Functor]): Monad[Free[F, ?]] = new Monad[Free[F, ?]] {
    override val bind = freeBind[F]
    override val applicative = new Applicative[Free[F, ?]] {
      override val apply = bind.apply
      override def pure[A](a: A): Free[F, A] = Free(Algebra.Thunk.pure[A](_ => a))
    }
  }
}

