package scato
package transformers

object State {
  def pure[S, A](a: A): State[S, A] = StateT.pure[S, Identity, A](a)
  def state[S, A](f: S => (A, S)): State[S, A] = StateT.state[S, Identity, A](f)
  def run[S, A](sa: StateT[S, Identity, A])(s: S): (A, S) = StateT.run[S, Identity, A](sa)(s).run

  def modify[S](f: S => S): StateT[S, Identity, Unit] = StateT.modify[S, Identity](f)
  def get[S]: StateT[S, Identity, S] = StateT.get[S, Identity]
  def exec[S, A](sa: StateT[S, Identity, A])(s: S): S = run(sa)(s)._2
}

