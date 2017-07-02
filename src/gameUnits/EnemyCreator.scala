package gameUnits

import scala.io.Source
import java.io._
import scala.collection.mutable.Buffer

object EnemyCreator {
  
  private class IllegalEnemyException(s: String) extends Exception(s)
  
  private val dir = new File("Enemies/")
  private val files = dir.listFiles()
  private val names = files.map(_.getName).map(_.takeWhile(_ != '.'))
  private val lines = for (file <- files)
    yield Source.fromFile(file).getLines().toArray.map(_.takeWhile(_ != '#').trim)
  
  val enemyTypes = Buffer[Enemy]()
  
  def createEnemies() = {
    require(lines.forall(_.length >= 3), "Not enough lines to create enemies")
    for (i <- 0 until lines.length) {
      try {
        val enemy = lines(i)
        class e extends Enemy {
          val name = names(i)
          val maxHealth = enemy(0).toInt
          val speed = enemy(1).toDouble
          val reward = enemy(2).toInt
          if (enemy.length > 3) cooldown = enemy(3).toInt
          var currentHealth = maxHealth
          def makeNew() = new e
        }
        enemyTypes += new e
      } catch {
          case e: NumberFormatException => throw new IllegalEnemyException("Can't convert letters to numbers")
      }
    }
  }
}