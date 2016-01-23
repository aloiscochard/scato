package scato
package clazz

trait MonadInstances {
  implicit val identity: Monad[Identity] = new Monad[Identity] {
    override val applicative = new Applicative[Identity] {
      override val apply = new Apply[Identity] {
        override val functor = new Functor[Identity] {
          override def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity(f(fa.run))
        }
        override def ap[A, B](fa: Identity[A])(f: Identity[A => B]): Identity[B] = Identity(f.run.apply(fa.run))
      }
      override def pure[A](a: A): Identity[A] = Identity(a)
    }

    override val bind = new Bind[Identity] {
      override def apply = applicative.apply
      override def flatMap[A, B](oa: Identity[A])(f: A => Identity[B]): Identity[B] = f(oa.run)
    }
  }
}
