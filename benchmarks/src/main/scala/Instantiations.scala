package scato
package benchmarks

import org.openjdk.jmh.annotations.{Benchmark, BenchmarkMode, Fork, Mode}

import Prelude._
import Data._

@Fork(1)
@BenchmarkMode(Array(Mode.Throughput))
class Instantiations {
  def l[F[_]](fi: F[Int])(implicit F: Light[F]): Functor[F] = F.functor
  def lTC[F[_]](fi: F[Int])(implicit F: TC[F, Light]): Functor[F] = F.instance.functor
  def lTCH[F[_]](fi: F[Int])(implicit F: TC[F, Light]): Functor[F] = {
    import Light.InstanceTC._
    clazz.Functor[F]
  }

  @Benchmark def lightDirect ={
    import Light.InstanceDirect._
    xs.map(i => l(List(i)))
  }

  @Benchmark def lightTC = {
    import Light.InstanceTC._
    xs.map(i => lTC(List(i)))
  }

  @Benchmark def lightTCHierarchy = {
    import Light.InstanceTC._
    xs.map(i => lTCH(List(i)))
  }

  def h[F[_]](fi: F[Int])(implicit F: Heavy[F]): Functor[F] = F.functor
  def hTC[F[_]](fi: F[Int])(implicit F: TC[F, Heavy]): Functor[F] = F.instance.functor
  def hTCH[F[_]](fi: F[Int])(implicit F: TC[F, Heavy]): Functor[F] = {
    import Heavy.InstanceTC._
    clazz.Functor[F]
  }

  @Benchmark def heavyDirect = {
    import Heavy.InstanceDirect._
    xs.map(i => h(List(i)))
  }

  @Benchmark def heavyTC = {
    import Heavy.InstanceTC._
    xs.map(i => hTC(List(i)))
  }

  @Benchmark def heavyTCHierarchy = {
    import Heavy.InstanceTC._
    xs.map(i => hTCH(List(i)))
  }


  @Benchmark def heavyTCCache = {
    import Heavy.InstanceTCCache._
    xs.map(i => hTC(List(i)))
  }
}

abstract class Light[F[_]] {
  def functor: Functor[F]
}

object Light {
  object InstanceDirect {
    implicit val functorList: Functor[List] = clazz.Monad.list.instance.bind.apply.functor
    implicit def light[F[_]](implicit F: Functor[F]): Light[F] = mkLight(F)
  }

  object InstanceTC {
    implicit def light[F[_]](implicit F: TC[F, Functor]): TC[F, Light] = TC[F, Light](mkLight(F.instance))

    implicit def lightFunctor[F[_]](implicit TC: TC[F, Light]): TC[F, Functor] =
      TC.map(new ~~>[Light, Functor] {
        override def apply[T[_]](mt: Light[T]): Functor[T] = mt.functor
      })
  }

  def mkLight[F[_]](F: Functor[F]): Light[F] = new Light[F] { val functor = F }
}

abstract class Heavy[F[_]] {
  def functor: Functor[F]
  def foo0: Heavy.Foo0[F]
}

object Heavy {
  object InstanceDirect {
    implicit val functorList: Functor[List] = clazz.Monad.list.instance.bind.apply.functor
    implicit def heavy[F[_]](implicit F: Functor[F]): Heavy[F] = mkHeavy(F)
  }

  object InstanceTC {
    implicit def heavy[F[_]](implicit F: TC[F, Functor]): TC[F, Heavy] = TC[F, Heavy](mkHeavy(F.instance))

    implicit def heavyFunctor[F[_]](implicit TC: TC[F, Heavy]): TC[F, Functor] =
      TC.map(new ~~>[Heavy, Functor] {
        override def apply[T[_]](mt: Heavy[T]): Functor[T] = mt.functor
      })
  }

  object InstanceTCCache {
    import TCCache._

    implicit def heavy[F[_]](implicit F: TC[F, Functor], ID: TypeTag[TC[F, Heavy]]): TC[F, Heavy] =
      capture[F, Heavy, TC[F, Heavy]](mkHeavy(F.instance))
  }

  def mkHeavy[F[_]](F: Functor[F]): Heavy[F] = new Heavy[F] {
    override val foo0 = new Foo0[F] {
      override val foo1 = new Foo1[F] {
        override val foo2 = new Foo2[F] {
          override val foo3 = new Foo3[F] {
            override val foo4 = new Foo4[F] {
              override val functor = F
            }
            override val functor = foo4.functor
          }
          override val functor = foo3.functor
        }
        override val functor = foo2.functor
      }
      override val functor = foo1.functor
    }
    override val functor = foo0.functor
  }

  abstract class Foo0[F[_]] {
    def functor: Functor[F]
    def foo1: Foo1[F]
  }

  abstract class Foo1[F[_]] {
    def functor: Functor[F]
    def foo2: Foo2[F]
  }

  abstract class Foo2[F[_]] {
    def functor: Functor[F]
    def foo3: Foo3[F]
  }

  abstract class Foo3[F[_]] {
    def functor: Functor[F]
    def foo4: Foo4[F]
  }

  abstract class Foo4[F[_]] {
    def functor: Functor[F]
  }
}
