package gameUnits

import scala.io.Source
import java.io._
import gameLogic._
import scala.collection.mutable.Buffer

object RoundCreator {
    
  private class IllegalRoundException(s: String) extends Exception(s)
  
  private val dir = new File("Rounds/")
  private val files = dir.listFiles()
  private val names = files.map(_.getName)
  private val lines = for (file <- files)
    yield Source.fromFile(file).getLines().toArray.map(_.takeWhile(_ != '#').trim)
  
  val rounds = Buffer[(String, Seq[Round])]()
  
  def createRounds() = {
    for (i <- 0 until lines.length) {
      val roundFile = lines(i) // The current file
      val name = names(i)      // Name of file
      val roundLines = Buffer[Buffer[String]]()  // Each of the rounds in the file (one Buffer[String] represents one round)
      val roundList = Buffer[Round]()            // The rounds in this file
      
      /* Chop the file into individual Buffers containing one round each and store them in roundLines */
      val currentRound = Buffer[String]()
      for (line <- roundFile) {
        if (!line.isEmpty) {
          currentRound += line
        } else {
          if (!currentRound.isEmpty) {
            roundLines += currentRound.clone
            currentRound.clear()
          }
        }
      }
      roundLines += currentRound.clone
      /* At this point, all rounds in a file are in Buffers */
      for (round <- roundLines) {
        val enemies = Buffer[(String, Int)]()
        for (line <- round) {
          try {
            val pair = line.split("/")
            val enemy = pair(0)
            val amount = pair(1).toInt
            enemies += ((enemy, amount))
          } catch {
            case e: NumberFormatException => throw new IllegalRoundException("Amount of enemies needs to be a number: " + line.split("/")(1))
            case e: IndexOutOfBoundsException => throw new IllegalRoundException("Couldn't create round, illegal line: " + line)
          }
        }
        roundList += Round(enemies)
      }
      rounds += ((name, roundList))
    }
  }
}