package scato
package io

import scala.concurrent.Future
import system.Unsafe.Val

object IOCore {
  case class  ThreadId(future: Future[Val], intr: Interruptor)
  case object ThreadKilled extends Error

  type Thunk = List[Exp]

  object Thunk {
    import Exp._
    import Val._

    def point[A](f: Unit => A): Thunk =
      Point(_ => cast(f(()))) :: Nil

    def bind[A, B](xs: Thunk)(f: A => IO[B]): Thunk =
      Bind(x => f(reify[A](x)).thunk) :: xs

    def map[A, B](xs: Thunk)(f: A => B): Thunk =
      Map(x => cast(f(reify[A](x)))) :: xs

    def apply[A, B, C](f: (A, B) => C)(ioa: IO[A], iob: IO[B]): Thunk =
      Apply((a, b) => cast((reify[A](a), reify[B](b))), ioa.thunk, iob.thunk) :: Nil

    def fork[A](io: IO[A]): Thunk =
      Fork(_ => io.thunk) :: Nil

    def catching[A, E <: Throwable](io: IO[A])(f: E => IO[A])(implicit E: scala.reflect.ClassTag[E]): Thunk =
      Catch({
        case e: E => Some(f(e).thunk)
        case _ => None
      }, io.thunk) :: Nil

    def fromFuture[A](future: Future[A]): Thunk =
      Wait(ThreadId(castF[Future, A](future), Interruptor.unintr)) :: Nil
  }

  sealed trait Exp

  object Exp {
    import Val._

    case class Point(f: Unit => Val) extends Exp
    case class Map(f: Val => Val) extends Exp
    case class Bind(f: Val => Thunk) extends Exp
    case class Apply(f: (Val, Val) => Val, left: Thunk, right: Thunk) extends Exp

    case class Fork(f: Unit => Thunk) extends Exp
    case class Wait(t: ThreadId) extends Exp

    case class Catch(f: Throwable => Option[Thunk], on: Thunk) extends Exp
  }
}
