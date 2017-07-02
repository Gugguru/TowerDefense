package gameUnits
import gameLogic.Coord

trait Tower {
  var name: String = "Tower"
  var location: Coord = (0,0)
  var range: Int
  var size: Int = 10
  val cost: Int
  def makeNew(loc: Coord): Tower
}
