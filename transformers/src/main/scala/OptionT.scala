package scato
package transformers

case class OptionT[F[_], A](run: F[Option[A]])

object OptionT extends OptionTInstances
