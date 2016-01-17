package scato
package io

import scala.concurrent.{Await, ExecutionContext, Future}
import scala.concurrent.duration.Duration

import system.Unsafe.Val

import Interruptor._
import IOCore._

case class RTS(ec: ExecutionContext, intr: Interruptor) {
  import RTS._
  import Interpreter._

  def unsafePerformIO[A](io: IO[A]): Future[A] =
    Val.reifyF[Future, A](unsafePerformEval(this)(io.thunk.reverse))

  def unsafePerformIO_[A](io: IO[A]): A =
    Await.result(unsafePerformIO(io), Duration.Inf)
}

object RTS {
  val globalInterruptor: Interruptor = new UnInterruptor { def newChild: Interruptor = newInterruptor }
  val defaultRTS: RTS = RTS(ExecutionContext.Implicits.global, globalInterruptor)

  def newInterruptor: Interruptor = new Interruptor {
    @volatile
    private var _interrupted = false
    private var _children: List[Interruptor] = Nil
    def kill: Unit = _interrupted = true
    def interrupted: Boolean = _interrupted
    def newChild: Interruptor = {
      val intr = newInterruptor
      _children = intr :: _children.filter(!_.interrupted)
      intr
    }
  }

  object Interpreter {

    type Continuation = Future[Val => Val]
    type Frame = Either[Thunk, Continuation]
    type Stack = List[Frame]
    type State = (Thunk, Stack)

    def unsafePerformEval(rts: RTS)(thunk: Thunk): Future[Val] =
      unsafePerformEval_(thunk, Val.unit, Nil, rts.intr)(rts.ec)

    def unsafePerformEval_(
      thunk0: Thunk,
      value0: Val,
      stack0: Stack,
      intr: Interruptor)
    (implicit ec: ExecutionContext): Future[Val] = {

      import Exp._
      import Val._

      var thunk: List[Exp] = thunk0
      var value: Val = value0
      var stack: List[Frame] = stack0
      var next: Option[Continuation] = None
      var catchers: List[(Throwable => Option[Thunk], State)] = Nil

      var done: Boolean = false

      def die = throw ThreadKilled

      while(!done) {
        def thunkEmpty = thunk.tail.isEmpty

        def setVal(x: Val): Unit = {
          done = thunkEmpty
          value = x
          stepThunk
        }

        def stepThunk(): Unit = if (!thunkEmpty) thunk = thunk.tail
        def pushC(x: Continuation): Unit = stack = Right(x) :: stack
        def pushT(): Unit = if (!thunkEmpty) stack = Left(thunk.tail) :: stack

        val tail = if (thunk.tail.nonEmpty) Some(thunk.tail) else None

        try {
          thunk.head match {
            case Point(f) => setVal(f(()))
            case Map(f)   => setVal(f(value))

            case Fork(f)  => setVal {
              val intrChild = intr.newChild
              def run = unsafePerformEval_(f(()).reverse, unit, Nil, intrChild)
              cast(ThreadId(Future(run).flatMap(identity), intrChild))
            }

            case Bind(f) =>
              pushT
              thunk = f(value).reverse

            case Apply(f, left, right) =>
              pushT
              thunk = left.reverse
              pushC(
                Future(unsafePerformEval_(right.reverse, unit, Nil, intr))
                  .flatMap(_.map(r => ((l: Val) => f(l, r))))
              )

            case Wait(ThreadId(future, tintr)) =>
              done = true
              if (tintr.interrupted) {
                stack = Nil
                thunk = Nil
                next = Some(Future(die))
              } else {
                pushT
                thunk = Nil
                pushC(future.map(x => (_: Val) => x))
              }

            case Catch(f, on) =>
              catchers = (f, (thunk.tail, stack)) :: catchers
              pushT
              thunk = on.reverse :+ Point { _ => catchers = catchers.tail; value }
          }
        } catch { case e: Throwable =>
          catchers.foldRight(None: Option[(Thunk, State)]) { case ((f, state), res) =>
            res.orElse(f(e).map((_, state)))
          } match {
            case Some((recovery, (stateThunk, stateStack))) =>
              done = false
              stack = stateStack
              value = cast(e)
              thunk = recovery.reverse ++ stateThunk
            case None => throw e
          }
        }

        // TODO Performance analysis of the volatile in the intr
        if ((!done || (done && stack.nonEmpty)) && intr.interrupted) die

        if (done && stack.nonEmpty) {
          stack.head match {
            case Left(head) =>
              done = false
              thunk =  head
            case Right(future) =>
              next = Some(future)
          }
          stack = stack.tail
        }
      }

      next.fold(Future.successful(value)) { future =>
        stack match {
          case Left(nextThunk)::tail => future.flatMap(f => unsafePerformEval_(nextThunk, f(value), tail, intr))
          case _                => future.map(f => f(value))
        }
      }
    }
  }
}
