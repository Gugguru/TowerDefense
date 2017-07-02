package gameUnits
import gameLogic.Coord

abstract class ShootingTower extends DamageTower {
  
  var target: TargetType = First
  
  /* Find the best target according to targeting logic */
  override def findTarget = {
    var targets = enemies.filter(e => 
      //(x - h)^2 - (y - k)^2 < r^2
      (this.location dist e.location) < range)
    if (!targets.isEmpty) target match {
      case First  => Some(Array(targets.minBy(_.distanceToGoal)))
      case Last   => Some(Array(targets.maxBy(_.distanceToGoal)))
      case Strong => Some(Array(targets.maxBy(_.currentHealth)))
      case Weak   => Some(Array(targets.minBy(_.currentHealth)))
    }
    else None
  }
}
