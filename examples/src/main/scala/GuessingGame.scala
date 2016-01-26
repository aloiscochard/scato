package scato
package examples

import Prelude._

import transformers.StateT

import io.{App, IO}
import IO.syntax.{putStrLn, getLine}

// TODO Add, Random, String/Int safe conv, Ordering
// from https://wiki.haskell.org/Simple_StateT_use
object GuessingGame extends App {

  def mainIO: IO[Unit] = for {
    _ <- putStrLn("I'm thinking of a number between 1 and 100, can you guess it?")
    n <- session(42).exec(0)
    _ <- putStrLn(s"Success in $n tries.")
  } yield ()


  def session(answer: Int): StateT[Int, IO, Unit] = {
    val StateT = transformers.StateT.syntax[Int, IO]
    import StateT._

    for {
      gs <- lift(getLine)
      _  <- modify(_ + 1)
      g = gs.toInt
      _  <- if (g == answer) {
        lift(putStrLn("Got it!"))
      } else {
        for {
          _ <- if (g > answer) {
              lift(putStrLn("Too High !"))
            } else {
              lift(putStrLn("Too Low !"))
            }
          _ <- session(answer)
        } yield ()
      }
    } yield ()
  }
}
