package scato
package transformers

object State {
  def state[S, A](f: S => (A, S)): State[S, A] = StateT.state[S, Identity, A](f)
}

