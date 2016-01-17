package scato
package examples

object BaseDemo {

  import clazz._
  import Functor.syntax._
  import Apply.syntax._
  import Bind.syntax._

  import data.{Disjunction, Maybe}
  import data.Disjunction._
  import data.Maybe._
  import transformers.OptionT

  def bar0[F[_], A, FA](xs: F[A])(implicit F: TC[F, Functor]): F[A] = {
    map(xs)(identity[A])
  }

  def bar1[F[_], A](xs: F[A])(implicit F: TC[F, Applicative]): F[A] = {
    map(xs)(identity[A])
  }

  def foo0[M[_], A](xs: M[A])(implicit M: TC[M, Monad]): M[A] =
    bar0(xs)

  def foo1[M[_], N[_], A, B](xs: M[A], ys: N[B])(implicit M: TC[M, Monad], N: TC[N, Monad]): (M[A], N[B]) =
    (bar0(xs), bar1(ys))

  def barU[F[_], A, FA](xs: FA)(implicit F: TCU[Functor, FA]): F.T[F.A] =
    F.instance.map(F(xs))(x => x)

  def withMaybe(): Unit = {
    val opt0: Maybe[Int] = Just(42)
    val opt1: Maybe[String] = Just("foo")

    map(bar0(opt0))(_ + 32)
    foo0(opt0)
    foo1(opt0, opt1)

    val opt01: Maybe[(Int, String)] = opt0 <*> opt1

    (opt01 *> opt0).flatMap(_ => opt0)
  }

  def withOption(): Unit = {
    val opt0: Option[Int] = Some(42)
    val opt1: Option[String] = Some("foo")

    map(bar0(opt0))(_ + 32)
    foo0(opt0)
    foo1(opt0, opt1)

    val opt01: Option[(Int, String)] = opt0 <*> opt1

    flatMap(opt01 *> opt0)(_ => opt0)
  }

  def withDisjunction(): Unit = {
    val ei0: String \/ Int = R_(42)
    println(ei0.map(_ + 54) <*> (ei0.map(_ - 10))(L_("error")))
  }

  def withOptionT(): Unit = {
    def some[A](a: A): Option[A] = Some(a)
    println(OptionT(Identity(some(42))).map(_ - 10).flatMap(_ => OptionT(Identity(some(32)))))
  }

  def withProfunctors(): Unit = {
    import profunctors._
    import Profunctor.syntax._

    val f: Int => Int = identity[Int]
    f.lmap[Double](_.toInt).rmap(_ / 2).apply(54.32)
  }

  def withTraversable(): Unit = {
    import Traversable.syntax._

    val xs: List[Int] = List(1, 2, 3)
    val f: Int => Option[Int] = i => if (i > 10) None else Some(i)

    println(xs.traverse(f))
  }
}
