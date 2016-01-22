package scato

package object transformers {
  type State[S, A] = StateT[S, Identity, A]
}
