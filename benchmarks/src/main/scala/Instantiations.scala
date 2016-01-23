package scato
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

import Prelude._
import Data._

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class Instantiations {
  import Hierarchy._

  def l[F[_]](fi: F[Int])(implicit F: Light[F]): Foldable[F] = F.foldable
  def lH[F[_]](fi: F[Int])(implicit F: Light[F]): Foldable[F] = Foldable[F]

  def h[F[_]](fi: F[Int])(implicit F: Heavy[F]): Foldable[F] = F.foldable
  def hH[F[_]](fi: F[Int])(implicit F: Heavy[F]): Foldable[F] = Foldable[F]

  @Benchmark def lightDirect = xs.map(i => l(List(i)))
  @Benchmark def lightHierarchy = xs.map(i => lH(List(i)))
  @Benchmark def heavyDirect = xs.map(i => h(List(i)))
  @Benchmark def heavyTCHierarchy = xs.map(i => hH(List(i)))
}

object Hierarchy {
  implicit def lightFoldable[F[_]](implicit F: Light[F]): Foldable[F] = F.foldable
  implicit def heavyFoldable[F[_]](implicit F: Heavy[F]): Foldable[F] = F.foldable
}

abstract class Light[F[_]] {
  def foldable: Foldable[F]
}

object Light {
  implicit def light[F[_]](implicit F: Functor[F]): Light[F] = new Light[F] { val foldable = null }
}

abstract class Heavy[F[_]] {
  def foldable: Foldable[F]
  def foo0: Heavy.Foo0[F]
}

object Heavy {
  implicit def heavy[F[_]](implicit F: Functor[F]): Heavy[F] = new Heavy[F] {
    override val foo0 = new Foo0[F] {
      override val foo1 = new Foo1[F] {
        override val foo2 = new Foo2[F] {
          override val foo3 = new Foo3[F] {
            override val foo4 = new Foo4[F] {
              override val foldable = null
            }
            override val foldable = foo4.foldable
          }
          override val foldable = foo3.foldable
        }
        override val foldable = foo2.foldable
      }
      override val foldable = foo1.foldable
    }
    override val foldable = foo0.foldable
  }

  abstract class Foo0[F[_]] {
    def foldable: Foldable[F]
    def foo1: Foo1[F]
  }

  abstract class Foo1[F[_]] {
    def foldable: Foldable[F]
    def foo2: Foo2[F]
  }

  abstract class Foo2[F[_]] {
    def foldable: Foldable[F]
    def foo3: Foo3[F]
  }

  abstract class Foo3[F[_]] {
    def foldable: Foldable[F]
    def foo4: Foo4[F]
  }

  abstract class Foo4[F[_]] {
    def foldable: Foldable[F]
  }
}
