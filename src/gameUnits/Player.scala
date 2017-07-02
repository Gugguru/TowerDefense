package gameUnits

/* A simple class to keep track of values */
class Player {
  
  var lives = 20
  var money = 150
  
  def addMoney(a: Int) = money += a
  def removeMoney(a: Int) = money -= a
  
}