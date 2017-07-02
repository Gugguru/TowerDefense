package gameUnits
import gameLogic._

trait Enemy {
  
  def makeNew(): Enemy
  val name: String
  
  /* Method explained in project document */
  def move: Unit = {
    def target = {
      while ((location dist path(pathTurn)) < speed && pathTurn + 1 < path.length) pathTurn += 1
      path(pathTurn)
    }
    def moveToTarget() = {
      val vector = (target - location)
      location = location + vector.makeLength(speed)
    }
    if (!((location dist target) < speed)) {
      moveToTarget
    } else if (pathTurn < path.length - 1) {
      pathTurn += 1
      moveToTarget
    } else {
      goalReached = true
    }
  }
  var pathTurn = 0
  
  def takeDmg(d: Int) = 
    currentHealth -= d
  
  def path: Seq[Coord] = Game.course.getPath 
  
  val maxHealth: Int
  var currentHealth: Int
  val speed: Double
  var location: Coord = (0,0)
  var goalReached = false
  val reward: Int
  var cooldown: Int = 60
  /* Size depends on enemy's max health, but is limited to 10-100 (100 - 1000hp) */
  def size = Math.max(Math.min(maxHealth / 10, 100), 10)
  
  /* Explained in project document */
  def distanceToGoal: Double = {
    val remainingPath = path.drop(pathTurn)
    var res = location dist path(pathTurn)
    for (i <- 0 until remainingPath.length - 1) {
      res += remainingPath(i + 1) dist remainingPath(i)
    }
    res
  }
  
}