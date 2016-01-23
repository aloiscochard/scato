package scato
package clazz

trait BindClass[M[_]] extends ApplyClass[M] { self =>
  final implicit val bind: Bind[M] = new Bind[M] {
    def apply: Apply[M] = self.apply
    def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B] = self.flatMap[A, B](ma)(f)
  }
  def flatMap[A, B](ma: M[A])(f: A => M[B]): M[B]
}
