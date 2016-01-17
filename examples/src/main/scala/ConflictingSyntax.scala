package scato
package examples

object ConflictingSyntax {
  // Let define a simple `Foo` type and with a polymorphic function over Functor for it.
  // So, we import the functor syntax...

  /*
  import scalaz.syntax.functor._

  case class Foo[A](run: A)

  def foo[F[_], A](xs: F[A])(implicit F: Functor[F]): F[Foo[A]] =
    xs.map(Foo(_))
  */

  // Now we add a function, but this time the fun is polymorphic over Applicative
  // We follow our instinct and import the applicative syntax as well...

  /*
  import scalaz.{Applicative, Functor}
  import scalaz.syntax.functor._
  import scalaz.syntax.applicative._

  case class Foo[A](run: A)

  def foo[F[_], A](xs: F[A])(implicit F: Functor[F]): F[Foo[A]] =
    xs.map(Foo(_))

  def bar[F[_]](implicit F: Applicative[F]): F[Foo[Int]] =
    foo(42.pure[F])
  */

  /* BOOM, the compiler refuse to compile this code and says:
   *
   *   value map is not a member of type parameter F[A]
   *     xs.map(Foo(_))
   *        ^
  */

  // Which is rather confusing as we imported both functor and applicative syntax so it must be there ...
  // ... but the syntax imports are conflicting.

  // Let's see if it works here.
  case class Foo[A](run: A)

  def syntax0(): Unit = {
    import clazz.{Applicative, Functor}
    import Functor.syntax._
    import Applicative.syntax._

    def foo[F[_], A](xs: F[A])(implicit F: TC[F, Functor]): F[Foo[A]] =
      xs.map(Foo(_))

    def bar[F[_]](implicit F: TC[F, Applicative]): F[Foo[Int]] =
      foo(42.pure[F])
  }

  // But what if we import only the Applicative syntax?
  // BOOM ... the compiler would refuse to compile the code with the same error as above.

  // That is because none of base typeclasses modules does any re-exporting,
  // which is a GoodThing and make the behavior more predictable.

  // It is instead advised though to use a `prelude` in your projects,
  // there exists a default one, let's see.

  def syntaxPrelude(): Unit = {
    import clazz._
    import Prelude._

    def foo[F[_], A](xs: F[A])(implicit F: TC[F, Functor]): F[Foo[A]] =
      xs.map(Foo(_))

    def bar[F[_]](implicit F: TC[F, Applicative]): F[Foo[Int]] =
      foo(42.pure[F])
  }

  // We might forgot what is in the prelude though, what if we import twice the same syntax now?
  /*
  def syntaxPreludeConflict(): Unit = {
    import clazz._
    import Prelude._
    import Functor.syntax._

    def foo[F[_], A](xs: F[A])(implicit F: TC[F, Functor]): F[Foo[A]] =
      xs.map(Foo(_))
  }
  */

  // BOOM ... but now we get this interesting note in the error message:
  /*
Note that implicit conversions are not applicable because they are ambiguous:
both method PfunctorOps in object Prelude of type ...
and method functorOps in trait FunctorSyntax of type ...
are possible conversion functions from xs.type to ?{def map: ?}
     xs.map(Foo(_))
     ^
     */

  // Which gives precise about information about the conflict.
}
