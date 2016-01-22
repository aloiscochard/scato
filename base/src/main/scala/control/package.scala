package scato

package object control {
  type Lazy[A] = Unit => A
}
