package day10

import java.lang.Integer.parseInt

object Solution {

  //  val data = Data.example
  val data = Data.data

  val pointRegex = raw"position=<\s*([-]?\d+),\s*([-]?\d+)> velocity=<\s*([-]?\d+),\s*([-]?\d+)>".r

  case class Point(x: Int, y: Int, dx: Int, dy: Int)

  def parse(data: String): List[Point] = data.split("\n")
    .map(_.trim)
    .map(_.replace("|", ""))
    .map(parsePoint)
    .toList

  def parsePoint(s: String): Point = s match {
    case pointRegex(x, y, dx, dy) => Point(parseInt(x), parseInt(y), parseInt(dx), parseInt(dy))
    case _ => throw new Error("No match for " + s)
  }

  def evolve(points: List[Point]): List[Point] = points match {
    case Nil => Nil
    case head :: tail => Point(head.x + head.dx, head.y + head.dy, head.dx, head.dy) :: evolve(tail)
  }

  def buildGrid(points: List[Point]): Array[Array[String]] = {
    val minX = points.minBy(_.x).x
    val minY = points.minBy(_.y).y

    val maxX = points.maxBy(_.x).x
    val maxY = points.maxBy(_.y).y

    val grid = Array.fill(maxY - minY + 1, maxX - minX + 1)(".")
    points.foreach(p => grid(p.y - minY)(p.x - minX) = "#")
    grid
  }

  def printGrid(grid: Array[Array[String]]): Unit = {
    println(grid.map(_.mkString).mkString("\n"))
    println()
  }

  def findGridSize(points: List[Point]): Long = {
    val minX = points.minBy(_.x).x
    val minY = points.minBy(_.y).y

    val maxX = points.maxBy(_.x).x
    val maxY = points.maxBy(_.y).y

    (maxX - minX) * (maxY - minY)
  }

  def main(args: Array[String]): Unit = {
    val points = parse(data)

    var grid = points
    val gridSizes = Array.fill(30000)(-1L)
    for (i <- 0 until 30000) {
      grid = evolve(grid)
      gridSizes(i) = findGridSize(grid)
    }

    val result = gridSizes.zipWithIndex.filterNot(_._1 < 0).minBy(_._1)
    //Answer to question 2 (minus 1 since we started at 0)
    println(result)

    grid = points
    for (_ <- 0 to result._2) {
      grid = evolve(grid)
    }
    //Answer to question 1
    printGrid(buildGrid(grid))
  }
}
