package day6

import scala.collection.mutable

object Solution {

  def main(args: Array[String]): Unit = {
    val data = Data.data

    val minX = data.map(_._1).min;
    val minY = data.map(_._2).min;

    val translatedData = data.map(
      { case (x, y) => (x - minX, y - minY) }
    )

    val maxX = translatedData.map(_._1).max;
    val maxY = translatedData.map(_._2).max;

    val grid = Array.ofDim[Int](maxX + 1, maxY + 1)
    for (x <- 0 to maxX) {
      for (y <- 0 to maxY) {
        grid(x)(y) = findLowestDistance(x, y, translatedData)
      }
    }

    println(grid.flatten.groupBy(identity).mapValues(_.size).maxBy(_._2))
  }

  def findLowestDistance(x: Int, y: Int, data: Seq[(Int, Int)]): Int = {
    var lowestDistance = Integer.MAX_VALUE
    var closestDanger = -1
    for (((dx, dy), i) <- data.zipWithIndex) {
      val distance = Math.abs(x - dx) + Math.abs(y - dy)
      if (distance == lowestDistance) {
        return -1
      }
      if (distance < lowestDistance){
        lowestDistance = distance
        closestDanger = i
      }
    }
    return closestDanger
  }
}
