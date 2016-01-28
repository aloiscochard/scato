package scato
package io

import scato.clazz.{BindClass, MonadClass}

import IO.syntax.constIO

trait IOInstances extends MonadClass[IO] with BindClass.Ap[IO] {
  override def flatMap[A, B](ioa: IO[A])(f: A => IO[B]): IO[B] = ioa.flatMap(f)
  override def map[A, B](ioa: IO[A])(f: A => B): IO[B] = ioa.map(f)
  override def pure[A](a: A): IO[A] = constIO[A](a)
}
