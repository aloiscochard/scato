package scato

object Control {
  type Lazy[A] = Unit => A
}
