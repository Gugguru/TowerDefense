package gui
import scala.swing.Label
import scala.collection.mutable.Buffer

/* A class that represents labels with text that needs to update often. The companion object keeps track
 * of all the instances created so it's easy to call updateText() for all instances simultaneously */

class TextLabel extends Label {
  def updateText(): Unit = 
    this.text = ""
  TextLabel.labels += this
}

object TextLabel {
  val labels: Buffer[TextLabel] = Buffer()
}