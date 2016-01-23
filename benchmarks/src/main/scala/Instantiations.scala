package scato
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}
import scato.Prelude._
import scato.benchmarks.Data._

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class Instantiations {

  def l[F[_]](fi: F[Int])(implicit F: Light[F]): Foldable[F] = F.foldable

  def h[F[_]](fi: F[Int])(implicit F: Heavy[F]): Foldable[F] = F.foo0.foo1.foldable

  def hs[F[_]](fi: F[Int])(implicit F: HeavySubtyping[F]): Foldable[F] = F.foo0.foo1.foldable

  @Benchmark def light = xs.map(i => (l(List(i))))
  @Benchmark def heavy = xs.map(i => (h(List(i))))
  @Benchmark def heavySubtyping = xs.map(i => (hs(List(i))))
}

object Hierarchy {
  implicit def lightFoldable[F[_]](implicit F: Light[F]): Foldable[F] = F.foldable
  implicit def heavyFoldable[F[_]](implicit F: Heavy[F]): Foldable[F] = F.foo0.foo1.foldable
  implicit def heavySubtypingFoldable[F[_]](implicit F: HeavySubtyping[F]): Foldable[F] = F.foo0.foo1.foldable
}

abstract class Light[F[_]] {
  def foldable: Foldable[F]
  def functor: Functor[F]
}

object Light {
  implicit def light[F[_]](implicit F: Functor[F]): Light[F] = new Light[F] { def foldable = null; val functor = F }
}


abstract class Heavy[F[_]] {
  def foo0: Heavy.Foo0[F]
  def foo3: Heavy.Foo3[F]
}

object Heavy {

  abstract class Foo0[F[_]] {
    def foo1: Foo1[F]
    def foo4: Foo4[F]
  }

  abstract class Foo1[F[_]] {
    def foldable: Foldable[F]
    def foo2: Foo2[F]
  }

  abstract class Foo2[F[_]] {
    def functor: Functor[F]
  }

  abstract class Foo3[F[_]] {
    def functor: Functor[F]
    def foo4: Foo4[F]
  }

  abstract class Foo4[F[_]] {
    def foldable: Foldable[F]
  }

  implicit def heavy[F[_]](implicit F: Functor[F]): Heavy[F] = new Heavy[F] {

    override val foo3 = new Foo3[F] {
      override val functor = F
      override val foo4 = new Foo4[F] {
        override val foldable = null;
      }
    }

    override val foo0 = new Foo0[F] {
      override val foo4: Foo4[F] = foo3.foo4
      override val foo1 = new Foo1[F] {
        override val foo2 = new Foo2[F] {
          override val functor = foo3.functor
        }
        override val foldable = foo4.foldable
      }

    }

  }
}


trait HeavySubtyping[F[_]] {
  def foo0: HeavySubtyping.Foo0[F]
  def foo3: HeavySubtyping.Foo3[F]
}


object HeavySubtyping {

  trait HeavySubtypingClass[F[_]] extends HeavySubtyping[F] with Foo0Class[F] with Foo3Class[F] {

  }

  trait Foo0[F[_]] {
    def foo1: Foo1[F]
    def foo4: Foo4[F]
  }

  trait Foo0Class[F[_]] extends Foo0[F] with Foo1Class[F] with Foo4Class[F] {
    final def foo0: Foo0[F] = this
  }

  trait Foo1[F[_]] {
    def foldable: Foldable[F]
    def foo2: Foo2[F]
  }

  trait Foo1Class[F[_]] extends Foo1[F] with Foo2Class[F] {
    def functor: Functor[F]
    final def foo1: Foo1[F] = this
  }

  trait Foo2[F[_]] {
    def functor: Functor[F]
  }

  trait Foo2Class[F[_]] extends Foo2[F] {
    def functor: Functor[F]
    final def foo2: Foo2[F] = this
  }

  trait Foo3[F[_]] {
    def functor: Functor[F]
    def foo4: Foo4[F]
  }

  trait Foo3Class[F[_]] extends Foo3[F] with Foo4Class[F]{
    def functor: Functor[F]
    final def foo3: Foo3[F] = this
  }

  trait Foo4[F[_]] {
    def foldable: Foldable[F]
  }

  trait Foo4Class[F[_]] extends Foo4[F] {
    def foldable: Foldable[F]
    final def foo4: Foo4[F] = this
  }

  implicit def heavySubtyping[F[_]](implicit F: Functor[F]): HeavySubtyping[F] = new HeavySubtypingClass[F] {
    override val functor = F
    override val foldable = null;
  }
}
