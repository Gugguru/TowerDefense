package gameUnits

import scala.collection.mutable.Buffer

/* Represents the types of targets a tower may have.
 * Is open to additions, for example Fast and Slow could easily be implemented here. */
trait TargetType {
  TargetType.types += this
}

case object First  extends TargetType
case object Last   extends TargetType
case object Strong extends TargetType
case object Weak   extends TargetType

object TargetType {
  val types = Buffer[TargetType]()
}