package scato
package io

import IOCore.Thunk

case class Concurrently[A](thunk: Thunk) extends AnyVal {
  def *>[B](par: Concurrently[B]): Concurrently[B] =
    Concurrent.concurrently(IO[A](thunk), IO[B](par.thunk)).map(_._2).concurrently
  def map[B](f: A => B): Concurrently[B] = Concurrently[B](Thunk.map(thunk)(f))

  def io: IO[A] = IO(thunk)
}

