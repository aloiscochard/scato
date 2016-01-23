package scato
package clazz

trait FoldableClass[F[_]] extends Foldable[F]{
  implicit final def foldable: Foldable[F] = this
}
