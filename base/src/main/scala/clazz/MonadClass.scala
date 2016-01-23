package scato
package clazz

trait MonadClass[M[_]] extends BindClass[M] with ApplicativeClass[M] { self =>
  final implicit val monad: Monad[M] = new Monad[M] {
    def applicative: Applicative[M] = self.applicative
    def bind: Bind[M] = self.bind
  }

  override def map[A, B](ma: M[A])(f: (A) => B): M[B] = flatMap(ma)(a => pure(f(a)))
}
