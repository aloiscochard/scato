package scato
package free

package object monad {
  type Free[S[_], A] = FreeT[S, Identity, A]
}
