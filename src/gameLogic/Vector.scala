package gameLogic

import Math._

class Vector(val x: Double, val y: Double) {
  
  
  def +(that: Vector) = Vector(this.x + that.x, this.y + that.y)
  def -(that: Vector) = Vector(this.x - that.x, this.y - that.y)
  def *(i: Double) = Vector(this.x * i, this.y * i)
  def makeLength(i: Double) = {
    require(length != 0)
    Vector(x * i / length, y * i / length)
  }
  
  def dotP(that: Vector): Double = this.x * that.x + this.y + that.y
  
  def length = sqrt(x * x + y * y)
  override def toString = "Vector(" + this.x + ", " + this.y + ")"
  
  def xI = x.toInt
  def yI = y.toInt
}

object Vector {
  def apply(x: Double, y: Double): Vector = new Vector(x, y)
}