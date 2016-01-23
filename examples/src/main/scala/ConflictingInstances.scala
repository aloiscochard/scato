package scato
package examples

object ConflictingInstancesSubTyping {
  // `Foo` typeclass, a superclass of both `Bar` and `Baz`.
  abstract class Foo[F[_]]
  object Foo { implicit val option: Foo[Option] = ??? }

  // `Bar` typeclass, have `Foo` has a superclass.
  abstract class Bar[F[_]] extends Foo[F]
  object Bar { implicit def all[F[_]](implicit F: Foo[F]): Bar[F] = ??? }

  // `Baz` typeclass, have `Foo` has a superclass.
  abstract class Baz[F[_]] extends Foo[F]
  object Baz { implicit def int[F[_]](implicit F: Foo[F]): Baz[F] = ??? }

  // `Wooz` newtype, used to override `Bar` behavior on a given value.
  case class Wooz[F[_], A](run: F[A])
  object Wooz {
    implicit def woozBar[F[_], A](implicit F: Foo[F]): Bar[Wooz[F, ?]] = ???
  }

  // `Waaz` newtype, used to override `Baz` behavior on a given value.
  case class Waaz[F[_], A](run: F[A])
  object Waaz {
    implicit def waazBaz[F[_], A](implicit F: Foo[F]): Baz[Waaz[F, ?]] = ???
  }

  // Note that in practice `Wooz` and `Waaz` would have shared the `Foo` instance using a trait and subtyping,
  // it is not required in this example as the implementations are left `undefined`.

  // A foo combinator
  def foo[F[_], A](f0: F[A], f1: F[A])(implicit F: Foo[F]): F[A] = ???

  // Now let's define an abstract function which use this combinator.
  /*
  def woozy[F[_], A](wooz: Wooz[F, A], waaz: Waaz[F, A])
    (implicit FBar: Bar[F], FBaz: Baz[F]): F[A] =
      foo[F, A](wooz.run, waaz.run)

  BOOM, which instance should the compiler choose? we know the instance are the same, but not the compiler.

   both value FBaz of type com.bestmile.Test.Baz[F]
   and value FBar of type com.bestmile.Test.Bar[F]
   match expected type com.bestmile.Test.Foo[F]
        foo[F, A](wooz.run, waaz.run)
                 ^
  */
}


object ConflictingInstancesTC {
  // So... instead of encoding our typeclass hierarchy with subtyping, we simply compose them.

  // Typeclasses
  abstract class Foo[F[_]]
  object Foo { implicit val option: Foo[Option] = ??? }

  abstract class Bar[F[_]]
  object Bar {
    implicit def all[F[_]](implicit F: Foo[F]): Bar[F] = ???
  }

  abstract class Baz[F[_]]
  object Baz { implicit def int[F[_]](implicit F: Foo[F]): Baz[F] = ??? }

  // Newtypes
  case class Wooz[F[_], A](run: F[A])
  object Wooz {
    implicit def woozBar[F[_], A](implicit F: Foo[F]): Bar[Wooz[F, ?]] = ???
  }

  case class Waaz[F[_], A](run: F[A])
  object Waaz {
    implicit def waazBar[F[_], A](implicit F: Foo[F]): Baz[Waaz[F, ?]] = ???
  }

  // We now encode the relationships, by giving some natural transformations over the typeclasses.

  // That's the only place where we use subtyping: to order the implicits priority and affect the resolution.
  trait Hierarchy0 {
    implicit def barFoo[F[_]](implicit F: Bar[F]): Foo[F] = ???
  }

  object Hierarchy extends Hierarchy0 {
    implicit def bazFoo[F[_]](implicit F: Baz[F]): Foo[F] = ???
  }

  // Combinators
  import Hierarchy._

  // A foo combinator
  def foo[F[_], A](f0: F[A], f1: F[A])(implicit F: Foo[F]): F[A] = ???

  // Now let's define an abstract function which use this combinator.
  def woozy[F[_], A](wooz: Wooz[F, A], waaz: Waaz[F, A])
    (implicit FBar: Bar[F], FBaz: Baz[F]): F[A] =
      foo[F, A](wooz.run, waaz.run)
}
