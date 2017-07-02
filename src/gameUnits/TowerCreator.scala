package gameUnits

import scala.io.Source
import java.io._
import scala.collection.mutable.Buffer
import gameLogic.Coord

object TowerCreator {
  
  private class IllegalTowerException(s: String) extends Exception(s)
  
  private val dir = new File("Towers/")
  private val files = dir.listFiles()
  private val names = files.map(_.getName)
  private val lines = for (file <- files)
    yield Source.fromFile(file).getLines().toArray.map(_.takeWhile(_ != '#').trim)
  
  val towerTypes = Buffer[Tower]()
  
  def createTowers() = {
    for (i <- 0 until lines.length) {
      val tower = lines(i)
      val name_ = names(i)
      var kind = ""
      var cost_ = 0
      var speed_ = 0
      var dmg_ = 0
      var range_ = 0
      var income_ = 0
      var size_ = 0
      try {
        /* Match the "ID" of the tower and proceed accordingly */
        tower(0) match {
          case "S" => kind = "S"
          case "A" => kind = "A"
          case "M" => kind = "M"
          case other: String => throw new IllegalTowerException("Illegal tower type: " + other)
        }
        kind match {
          case "S" | "A" => {
            cost_ = tower(1).toInt
            speed_ = tower(2).toInt
            dmg_ = tower(3).toInt
            range_ = tower(4).toInt
            if (tower.length > 5) size_ = tower(5).toInt
          }
          case "M" => {
            cost_ = tower(1).toInt
            income_ = tower(2).toInt
            if (tower.length > 3) size_ = tower(3).toInt
          }
        }
        kind match {
          case "S" => {
            class t extends ShootingTower {
              name = name_
              val cost = cost_
              var speed = speed_
              var dmg = dmg_
              var range = range_
              if (size_ != 0) size = size_
              def makeNew(loc: Coord) = new t {location = loc}
            }
            towerTypes += new t
          }
          case "A" => {
            class t extends AreaTower {
              name = name_
              val cost = cost_
              var speed = speed_
              var dmg = dmg_
              var range = range_
              if (size_ != 0) size = size_
              def makeNew(loc: Coord) = new t {location = loc}
            }
            towerTypes += new t
          }
          case "M" => {
            class t extends MoneyTower {
              name = name_
              val cost = cost_
              val income = income_
              if (size_ != 0) size = size_
              def makeNew(loc: Coord) = new t {location = loc}
            }
            towerTypes += new t
          }
        }
      } catch {
          case e: IndexOutOfBoundsException => throw new IllegalTowerException("Not enough lines to create a tower")
          case e: NumberFormatException => throw new IllegalTowerException("Can't convert letters to numbers")
      }
    }
  }
}