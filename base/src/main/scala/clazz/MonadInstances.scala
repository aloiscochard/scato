package scato
package clazz

trait MonadInstances {
  implicit val identity: Monad[Identity] = new Monad[Identity] {
    override val applicative = new Applicative[Identity] {
      override val apply = new Apply[Identity] {
        override val functor = new Functor[Identity] {
          override def map[A, B](fa: Identity[A])(f: A => B): Identity[B] = Identity(f(fa.run))
        }
        override def ap[A, B](fa: Identity[A], f: Identity[A => B]): Identity[B] = Identity(f.run.apply(fa.run))
      }
      override def pure[A](a: A): Identity[A] = Identity(a)
    }

    override val bind = new Bind[Identity] {
      override def apply = applicative.apply
      override def flatMap[A, B](oa: Identity[A])(f: A => Identity[B]): Identity[B] = f(oa.run)
    }
  }

  implicit val option: Monad[Option] = new Monad[Option] {
    override val applicative = new Applicative[Option] {
      override val apply = new Apply[Option] {
        override val functor = new Functor[Option] {
          override def map[A, B](oa: Option[A])(f: A => B): Option[B] = oa.map(f)
        }
        override def ap[A, B](oa: Option[A], f: Option[A => B]): Option[B] = oa.flatMap(a => f.map(_(a)))
      }
      override def pure[A](a: A): Option[A] = Some(a)
    }

    override val bind = new Bind[Option] {
      override def apply = applicative.apply
      override def flatMap[A, B](oa: Option[A])(f: A => Option[B]): Option[B] = oa.flatMap(f)
    }
  }
}
