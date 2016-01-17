package scato

import clazz._

class TCBase extends TCBase.TCBase0

object TCBase {
  trait TCBase0 extends TCBase1 {
    implicit def monadBind[M[_]](implicit TC: TC[M, Monad]): TC[M, Bind] =
      TC.map(new ~~>[Monad, Bind] {
        override def apply[T[_]](mt: Monad[T]): Bind[T] = mt.bind
      })

    implicit def monadApplicative[M[_]](implicit TC: TC[M, Monad]): TC[M, Applicative] =
      TC.map(new ~~>[Monad, Applicative] {
        override def apply[T[_]](mt: Monad[T]): Applicative[T] = mt.applicative
      })

    implicit def monadApply[M[_]](implicit TC: TC[M, Monad]): TC[M, Apply] =
      TC.map(new ~~>[Monad, Apply] {
        override def apply[T[_]](mt: Monad[T]): Apply[T] = mt.applicative.apply
      })

    implicit def monadFunctor[M[_]](implicit TC: TC[M, Monad]): TC[M, Functor] =
      TC.map(new ~~>[Monad, Functor] {
        override def apply[T[_]](mt: Monad[T]): Functor[T] = mt.applicative.apply.functor
      })
  }

  trait TCBase1 extends TCBase2 {
    implicit def applicativeApply[M[_]](implicit TC: TC[M, Applicative]): TC[M, Apply] =
      TC.map(new ~~>[Applicative, Apply] {
        override def apply[T[_]](mt: Applicative[T]): Apply[T] = mt.apply
      })

    implicit def applicativeFunctor[M[_]](implicit TC: TC[M, Applicative]): TC[M, Functor] =
      TC.map(new ~~>[Applicative, Functor] {
        override def apply[T[_]](mt: Applicative[T]): Functor[T] = mt.apply.functor
      })
  }

  trait TCBase2 {
    implicit def applyFunctor[M[_]](implicit TC: TC[M, Apply]): TC[M, Functor] =
      TC.map(new ~~>[Apply, Functor] {
        override def apply[T[_]](mt: Apply[T]): Functor[T] = mt.functor
      })
  }
}
