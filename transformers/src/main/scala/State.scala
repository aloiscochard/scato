package scato
package transformers

object State {
  def pure[S, A](a: A): State[S, A] = StateT.pure[S, Identity, A](a)
  def state[S, A](f: S => (A, S)): State[S, A] = StateT.state[S, Identity, A](f)
  def run[S, A](sa: StateT[S, Identity, A])(s: S): (A, S) = sa.run(s).run
}

