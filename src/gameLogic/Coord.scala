package gameLogic

class Coord(val x: Double, val y: Double) {
  
  def +(that: Vector) = Coord(this.x + that.x, this.y + that.y)
  def -(that: Vector) = Coord(this.x - that.x, this.y - that.y)
  def -(that: Coord) = Vector(this.x - that.x, this.y - that.y)
  def ==(that: Coord) = this.x == that.x && this.y == that.y
  
  def dist(that: Coord): Double = (this - that).length
  
  def xI = x.toInt
  def yI = y.toInt
  
  override def toString = "(" + x + ", " + y + ")"
}

object Coord {
  def apply(x: Double, y: Double): Coord = new Coord(x, y)
  implicit def pairToCoord(pair: (Double, Double)) = Coord(pair._1, pair._2)
  implicit def intToCoord(pair: (Int, Int)) = Coord(pair._1.toDouble, pair._2.toDouble)
}