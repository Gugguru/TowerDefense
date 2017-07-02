package gameUnits

import scala.io.Source
import java.io._
import scala.collection.mutable.Buffer
import gameLogic._

object CourseCreator {
  
  RoundCreator.createRounds()
  val rounds = RoundCreator.rounds
  
  private class IllegalCourseException(s: String) extends Exception(s)
  
  private val dir = new File("Courses/")
  private val files = dir.listFiles()
  private val names = files.map(_.getName)
  private val lines = for (file <- files)
    yield Source.fromFile(file).getLines().toArray.map(_.takeWhile(_ != '#').trim)
  
  val courses = Buffer[Course]()
  
  def createCourses() = {
    for (i <- 0 until lines.length) {
      val course = lines(i)
      val name = names(i)
      var pic = course(0)
      var roundName = course(1)
      var roundList = rounds.find(_._1 == roundName).map(r => r._2.map(_.makeNew())).getOrElse(Seq[Round]())
      var enemyPath: Buffer[Coord] = Buffer()
      for (line <- course.drop(2)) {
        try {
          val coords = line.split(",")
          coords.foreach(_.trim)
          enemyPath += Coord(coords(0).toDouble, coords(1).toDouble)
        } catch {
          case e: NumberFormatException => throw new IllegalCourseException("Can't convert to number: " + line)
        }
      }
      courses += new Course(pic, enemyPath, roundList, name)
    }
  }
}