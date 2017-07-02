package gui
import scala.swing._
import gameUnits._
import java.awt.Color


/* A helper object that implements most of the drawing methods used in MainWindow */

object Drawing {
  
  /** Draws a square whose middle point is at the coordinates given by x and y */
  def drawTower(g: Graphics2D, x: Int, y: Int, size: Int, rangeEnabled: Boolean = false, range: Int = 0) {
    g.fillRect(x - size / 2, y - size / 2, size, size)
    if (rangeEnabled)
      g.drawOval(x - range, y - range, range * 2, range * 2)
  }
  
  def setTowerColor(g: Graphics2D, t: Tower) = {
    t match {
      case s: ShootingTower => g.setColor(Color.BLUE)
      case a: AreaTower => g.setColor(Color.BLACK)
      case m: MoneyTower => g.setColor(Color.GREEN)
    }
  }
  
  def drawShot(g: Graphics2D, x1: Int, y1: Int, x2: Int, y2: Int) {
    g.drawLine(x1, y1, x2, y2)
  }
  
  def drawArea(g: Graphics2D, x: Int, y: Int, range: Int) {
    g.setColor(new Color(0, 0, 0, 100))
    g.fillOval(x - range, y - range, range * 2, range * 2)
    g.setColor(Color.BLACK)
    g.drawOval(x - range, y - range, range * 2, range * 2)
  }
  
  /** Draws a (w x h) grid with wdist and hdist pixels between lines starting at (xStart, yStart) */
  def drawGrid(g: Graphics2D, w: Int, h: Int, wdist: Int, hdist: Int, xStart: Int, yStart: Int) {
    for (i <- 1 until h) {
      g.drawLine(xStart, yStart + i * hdist, xStart + w * wdist, yStart + i * hdist)
    }
    for (i <- 1 until w) {
      g.drawLine(xStart + i * wdist, yStart, xStart + i * wdist, yStart + h * hdist)
    }
  }
  
  def drawEnemy(g: Graphics2D, x: Int, y: Int, maxHP: Int, currentHP: Int, size: Int) {
    g.setColor(Color.RED)
    g.fillRect(x - size, y - size, 2 * size, 5)
    g.setColor(Color.GREEN)
    g.fillRect(x - size, y - size, 2 * size * currentHP / maxHP, 5)
    g.setColor(Color.BLACK)
    g.fillOval(x - size / 2, y - size / 2, size, size)
    
  }
}