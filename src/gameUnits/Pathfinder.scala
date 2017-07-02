package gameUnits

class Pathfinder extends Enemy {
  val maxHealth = 0
  val speed = 1.0
  val reward = 0
  var currentHealth = 0
  val name = "Pathfinder"
  def makeNew() = new Pathfinder
}