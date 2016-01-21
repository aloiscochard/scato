package scato

import clazz._

import scala.language.implicitConversions

trait Prelude extends data.DisjunctionFunctions {
  type \/[L, R] = data.Disjunction.\/[L, R]

  // FunctorSyntax
  implicit def PfunctorOps[F[_], A](fa: F[A])(implicit F: TC[F, Functor]): FunctorSyntax.Ops[F, A] =
    new FunctorSyntax.Ops(fa)(F.instance)

  implicit def PfunctorOpsU[F[_], FA](fa: FA)(implicit F: TCU[Functor, FA]): FunctorSyntax.Ops[F.T, F.A] =
    new FunctorSyntax.Ops(F(fa))(F.instance)

  // ApplySyntax
  implicit def PapplyOps[F[_], A](fa: F[A])(implicit F: TC[F, Apply]): ApplySyntax.Ops[F, A] =
    new ApplySyntax.Ops(fa)(F.instance)

  implicit def PapplyOpsU[F[_], FA](fa: FA)(implicit F: TCU[Apply, FA]): ApplySyntax.Ops[F.T, F.A] =
    new ApplySyntax.Ops[F.T, F.A](F(fa))(F.instance)

  // BindSyntax
  implicit def PbindOps[M[_], A](ma: M[A])(implicit M: TC[M, Bind]): BindSyntax.Ops[M, A] =
    new BindSyntax.Ops(ma)(M.instance)

  implicit def PbindOpsU[M[_], MA](ma: MA)(implicit M: TCU[Bind, MA]): BindSyntax.Ops[M.T, M.A] =
    new BindSyntax.Ops(M(ma))(M.instance)

  // ApplicativeSyntax
  implicit def PapplicativeOpsA[A](a: A): ApplicativeSyntax.OpsA[A] = new ApplicativeSyntax.OpsA(a)

  // TraversableSyntax
  implicit def traversableOps[T[_], A](ta: T[A])(implicit T: TC[T, Traversable]): TraversableSyntax.Ops[T, A] =
    new TraversableSyntax.Ops(ta)(T.instance)

  implicit def traversable[T[_], TA](ta: TA)(implicit T: TCU[Traversable, TA]): TraversableSyntax.Ops[T.T, T.A] =
    new TraversableSyntax.Ops(T(ta))(T.instance)
}

object Prelude extends Prelude
