package gameUnits
import gameLogic.{ Coord, Game }

abstract class DamageTower extends Tower {
  
  /* Keeps the last targets and returns them for the next 'shotFrames' amount of ticks so shots are more visible */
  def tick: Option[Seq[Enemy]] = {
    if (tickCounter > 0) {
      tickCounter -= 1
      
      // While not shooting, return the last target until the counter reaches 0
      if (keepTargetCounter > 0) {
        keepTargetCounter -= 1
        lastTarget
      } else None
      
    }
    else {
      // After a shot, reset counters and update lastTarget
      shoot.foreach{e => 
        tickCounter = speed
        keepTargetCounter = shotFrames
        lastTarget = Some(e)
        return Some(e)}
      None
    }
  }
  
  // Only used in ShootingTowers
  var lastTarget: Option[Seq[Enemy]] = None
  
  private var shotFrames = 5
  private var keepTargetCounter = shotFrames
  
  /* Potentially returns multiple targets. 
   * If a subclass only targets one enemy, this method is overridden in the subclass */
  def findTarget: Option[Seq[Enemy]] = {
    var targets = enemies.filter(e => 
      //(x - h)^2 - (y - k)^2 < r^2
      (this.location dist e.location) < range)
    if (!targets.isEmpty) Some(targets)
    else None
  }
  
  def shoot: Option[Seq[Enemy]] = {
    findTarget.foreach(e => e.foreach(_.takeDmg(dmg)))
    findTarget
  }
  
  def enemies: Seq[Enemy] = Game.course.enemies
  var speed: Int
  var dmg: Int
  private var tickCounter = speed
}