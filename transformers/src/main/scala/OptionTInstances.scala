package scato
package transformers

import clazz._
import Functor.syntax._
import Applicative.syntax._
import Bind.syntax._

trait OptionTInstances { instances =>
  implicit def functor[F[_]](implicit F: TC[F, Functor]): Functor[OptionT[F, ?]] = new Functor[OptionT[F, ?]] {
    override def map[A, B](oa: OptionT[F, A])(f: A => B): OptionT[F, B] =
      OptionT(oa.run.map(_.map(f)))
  }

  implicit def monad[F[_]](implicit F: TC[F, Monad]): Monad[OptionT[F, ?]] = new Monad[OptionT[F, ?]] {
    override val applicative = new Applicative[OptionT[F, ?]] {
      override val apply = new Apply[OptionT[F, ?]] {
        override val functor = instances.functor[F]
        override def ap[A, B](oa: OptionT[F, A])(of: OptionT[F, A => B]): OptionT[F, B] =
          OptionT(of.run.flatMap(_.fold(Applicative[F].pure[Option[B]](None))(f => oa.run.map(_.map(f)))))
      }
      override def pure[A](a: A): OptionT[F, A] =
        OptionT(Applicative[F].pure(None))
    }

    override val bind = new Bind[OptionT[F, ?]] {
      override val apply = applicative.apply
      override def flatMap[A, B](oa: OptionT[F, A])(f: A => OptionT[F, B]): OptionT[F, B] =
        OptionT(oa.run.flatMap(_.fold(Applicative[F].pure[Option[B]](None))(a => f(a).run)))
    }
  }

  implicit def applicativeTC[F[_], A](implicit F: TC[F, Monad]): TC[OptionT[F, ?], Applicative] =
    TC[OptionT[F, ?], Applicative](monad.applicative)
  implicit def applyTC[F[_], A](implicit F: TC[F, Monad]): TC[OptionT[F, ?], Apply] =
    TC[OptionT[F, ?], Apply](monad.applicative.apply)
  implicit def functorTC[F[_], A](implicit F: TC[F, Functor]): TC[OptionT[F, ?], Functor] =
    TC[OptionT[F, ?], Functor](functor)
  implicit def bindTC[F[_], A](implicit F: TC[F, Monad]): TC[OptionT[F, ?], Bind] =
    TC[OptionT[F, ?], Bind](monad.bind)
}
