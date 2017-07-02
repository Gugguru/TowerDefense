package gameLogic

import gameUnits._
import scala.collection.mutable.Buffer

class Round(name: String, n: Int) {
  
  def apply(n: Int) = enemies(n)
  
  val enemy = EnemyCreator.enemyTypes.find(_.name == name)
  var enemies: Seq[Enemy] = Buffer()
  
  enemy.foreach(e => enemies ++= (1 to n).map(i => e.makeNew()))  //Initialize the value of enemies
  
  def makeNew() = {
    val enemiesCopy: Seq[Enemy] = enemies.map(_.makeNew())
    new Round("", 0) { enemies = enemiesCopy }
  }
}

object Round {
  def apply(pairs: Seq[(String, Int)]): Round = {
    val list = Buffer[Enemy]()
    for (p <- pairs) {
      val enemy = EnemyCreator.enemyTypes.find(_.name == p._1)
      enemy.foreach(e => list ++= (1 to p._2).map(i => e.makeNew()))
    }
    new Round("", 0) {
      enemies = list
    }
  }
}