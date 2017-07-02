package gui
import scala.swing._
import gameUnits.TargetType
import scala.collection.mutable.Buffer

/* This class and its companion object are used to help handle tower targeting */

class TargetButton(val target: TargetType)(desc: String) extends ToggleButton {
  TargetButton.buttons += this
  preferredSize = new Dimension(50, 25)
  text = desc
  enabled = false
}

object TargetButton {
  val buttons = Buffer[TargetButton]()
}