package gameLogic

import java.awt.event.ActionListener
import java.awt.event.ActionEvent
import gameUnits._
import gui._
import javax.swing.Timer

object Game {
  
  CourseCreator.createCourses()
  
  val tickSpeed = 1000 / 60    //approx. 60 ticks per second
  
  var rounds = RoundCreator.rounds
  var courses: Seq[Course] = CourseCreator.courses.take(15)
  var course = new Course("Pics/bg.png", Array[Coord]() , Array(new Round("", 0)), "") //Default round, never used
  var player = new Player
  
  def win = course.isDone
  def loss = player.lives <= 0
  def isOver = (loss || win)
  
  def canAfford(t: Tower) = player.money >= t.cost
  
  val listener = new ActionListener() {
    def actionPerformed(e: ActionEvent) = {
      if (!isOver) {
        val (livesLost, towerMap, moneyEarned) = course.update
        player.lives -= livesLost
        player.money += moneyEarned
        MainWindow.towerShot = towerMap
        MainWindow.update()
      } else if (win) MainWindow.gameOver(true)
        else MainWindow.gameOver(false)
    }
  }
  var timer = new Timer(tickSpeed, listener)
  
}