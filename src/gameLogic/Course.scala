package gameLogic

import gameUnits._
import scala.collection.mutable.Buffer

class Course(bgPath: String, enemyPath: Seq[Coord], roundList: Seq[Round], val name: String) {
  
  def makeNew(): Course = new Course(bgPath, enemyPath, roundList, name)
  
  private var background: javax.swing.ImageIcon = new javax.swing.ImageIcon(bgPath)
  def getBackground = background
  
  private var path: Seq[Coord] = enemyPath
  def getPath = path
  

  private var currentRound = 0
  def roundNo = currentRound
  
  var autoStartRounds = false
  def isDone = roundNo == rounds.length && roundIsOver
  def roundIsOver = enemies.isEmpty && enemiesToCome.isEmpty
  
  var rounds:  Seq[Round] = roundList.map(_.makeNew)
  
  var enemies: Buffer[Enemy] = Buffer()
  var towers:  Buffer[Tower] = Buffer()
  def dmgTowers:   Buffer[DamageTower] = towers.collect{ case d: DamageTower => d }
  def moneyTowers: Buffer[MoneyTower]  = towers.collect{ case m: MoneyTower  => m }
  
  private val enemiesToCome: Buffer[Enemy] = Buffer()
  private var deploySpeed = 60    //1 enemy per sec
  private var ticker = 0
  
  /* Sends an enemy to the field if the timer is 0 and there are enemies left, otherwise reduces timer */
  private def deployEnemy = {
    if (!enemiesToCome.isEmpty) {
      if (ticker == 0) {
        enemies += enemiesToCome.head
        enemiesToCome.head.location = path(0)
        ticker = enemiesToCome.head.cooldown
        enemiesToCome -= enemiesToCome.head
      } else ticker -= 1
    } else ticker = 0
  }

  /* Returns all the points in the path that are 1 pixel apart */
  def pathPoints: Seq[Coord] = {
    var res = Buffer[Coord]()
    res += path.head
    var e = new Pathfinder
    while (!e.goalReached) {
      e.move
      res += e.location
    }
    res
  }
  
  /* Starts a new round and returns the amount of money earned */
  def startRound: Int = {
    if (currentRound < rounds.length) {
      rounds(currentRound).enemies.foreach(addEnemy(_))
      currentRound += 1
      moneyTowers.map(_.income).sum
    } else {
      0
    }
  }
  
  /* Checks whether the mouse is on a tower, returns the tower wrapped in an Option */
  def isOnTower(c: Coord): Option[Tower] = {
    towers.find(t => (c.x + t.size / 2 >= t.location.x && c.x - t.size / 2 <= t.location.x) &&
                       (c.y + t.size / 2 >= t.location.y && c.y - t.size / 2 <= t.location.y))
  }
  
  /* Checks whether the tower given as parameter can be placed at the current mouse position */
  def isLegal(c: Coord, s: Tower): Boolean = {
    towers.forall(t => (c dist t.location) > (t.size + s.size) / 2 + 1) &&
    pathPoints.forall(p => (c dist p) > s.size / 2 + 15)
  }

  /* Updates everything in the playfield
   * Returns how many lives have been lost and the target(s) of each tower */
  def update = {
    var moneyEarned = 0
    if (roundIsOver && autoStartRounds) moneyEarned = startRound
    deployEnemy
    enemies.foreach(_.move)
    val livesLost = enemies.filter(_.goalReached).map(e => (e.currentHealth / 100.0).ceil.toInt).sum
    val towerShot = dmgTowers.filter(_.tick.isDefined)    // Returns all towers that need a shot drawn
    val shotMap = for (i <- towerShot) yield (i, i.lastTarget.get)  // Returns all shooting towers with their targets
    moneyEarned += enemies.filter(_.currentHealth <= 0).map(_.reward).sum
    enemies = enemies.filter(e => e.goalReached == false && e.currentHealth > 0)
    (livesLost, shotMap, moneyEarned)
  }
  
  def addEnemy(e: Enemy) = enemiesToCome += e
  def addTower(t: Tower) = towers += t
  def removeTower(t: Tower) = towers -= t
}