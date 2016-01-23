package scato

import clazz._

import scala.language.implicitConversions

// TODO Add `MaybeFunctions` and syntax
trait Prelude extends data.DisjunctionFunctions {
  // Core Class
  // ==========
  type Applicative[F[_]] = clazz.Applicative[F]
  type Apply[F[_]] = clazz.Apply[F]
  type Bind[M[_]] = clazz.Bind[M]
  type Foldable[T[_]] = clazz.Foldable[T]
  type Functor[F[_]] = clazz.Functor[F]
  type Monad[M[_]] = clazz.Monad[M]
  type Traversable[T[_]] = clazz.Traversable[T]

  def Applicative[F[_]](implicit F: Applicative[F]): Applicative[F] = F
  def Apply[F[_]](implicit F: Apply[F]): Apply[F] = F
  def Bind[F[_]](implicit F: Bind[F]): Bind[F] = F
  def Foldable[F[_]](implicit F: Foldable[F]): Foldable[F] = F
  def Functor[F[_]](implicit F: Functor[F]): Functor[F] = F
  def Monad[M[_]](implicit M: Monad[M]): Monad[M] = M
  def Traversable[T[_]](implicit T: Traversable[T]): Traversable[T] = T

  // ApplicativeSyntax
  implicit def PapplicativeOpsA[A](a: A): ApplicativeSyntax.OpsA[A] = new ApplicativeSyntax.OpsA(a)

  // ApplySyntax
  implicit def PapplyOps[F[_], A](fa: F[A])(implicit F: Apply[F]): ApplySyntax.Ops[F, A] =
    new ApplySyntax.Ops(fa)

  implicit def PapplyOpsU[F[_], FA](fa: FA)(implicit F: Unapply[Apply, FA]): ApplySyntax.Ops[F.T, F.A] =
    new ApplySyntax.Ops[F.T, F.A](F(fa))(F.instance)

  // BindSyntax
  implicit def PbindOps[M[_], A](ma: M[A])(implicit M: Bind[M]): BindSyntax.Ops[M, A] =
    new BindSyntax.Ops(ma)

  implicit def PbindOpsU[M[_], MA](ma: MA)(implicit M: Unapply[Bind, MA]): BindSyntax.Ops[M.T, M.A] =
    new BindSyntax.Ops(M(ma))(M.instance)

  // FoldableSyntax
  implicit def PfoldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]): FoldableSyntax.Ops[F, A] =
    new FoldableSyntax.Ops(fa)

  implicit def PfoldableOpsU[F[_], FA](fa: FA)(implicit F: Unapply[Foldable, FA]): FoldableSyntax.Ops[F.T, F.A] =
    new FoldableSyntax.Ops(F(fa))(F.instance)

  // FunctorSyntax
  implicit def PfunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]): FunctorSyntax.Ops[F, A] =
    new FunctorSyntax.Ops(fa)

  implicit def PfunctorOpsU[F[_], FA](fa: FA)(implicit F: Unapply[Functor, FA]): FunctorSyntax.Ops[F.T, F.A] =
    new FunctorSyntax.Ops(F(fa))(F.instance)

  // TraversableSyntax
  implicit def PtraversableOps[T[_], A](ta: T[A])(implicit T: Traversable[T]): TraversableSyntax.Ops[T, A] =
    new TraversableSyntax.Ops(ta)

  implicit def traversable[T[_], TA](ta: TA)(implicit T: Unapply[Traversable, TA]): TraversableSyntax.Ops[T.T, T.A] =
    new TraversableSyntax.Ops(T(ta))(T.instance)


  // Core Data
  // =========

  type \/[L, R] = data.Disjunction.\/[L, R]
  type Maybe[A] = data.Maybe[A]
}

object Prelude extends Prelude
