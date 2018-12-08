package day6

import scala.collection.mutable.ListBuffer

object Solution {

  def main(args: Array[String]): Unit = {
    val data = Data.data
    //    val data = Seq((1, 1),
    //      (1, 6),
    //      (8, 3),
    //      (3, 4),
    //      (5, 5),
    //      (8, 9))


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

    val infiniteAreas1 = (0 to maxX)
      .flatMap(x => Stream(grid(x)(0), grid(x)(maxY)))
    val infiniteAreas2 = Stream.concat(grid(0).toStream, grid(maxX).toStream)
    val infiniteAreas = Stream.concat(infiniteAreas1, infiniteAreas2).distinct.toList

    println(infiniteAreas)

    println(grid.flatten
      .groupBy(identity)
      .mapValues(_.size)
      .filter(x => !infiniteAreas.contains(x._1))
      .maxBy(_._2))

    //Second half
    val distances = Array.ofDim[Int](maxX + 1, maxY + 1)
    for (x <- 0 to maxX) {
      for (y <- 0 to maxY) {
        distances(x)(y) = translatedData.map({ case (dx, dy) => Math.abs(x - dx) + Math.abs(y - dy) }).sum
      }
    }

    println(distances.toStream.flatMap(_.toStream).count(_ < 10000))
  }

  def findLowestDistance(x: Int, y: Int, data: Seq[(Int, Int)]): Int = {
    var lowestDistance = Integer.MAX_VALUE
    val closestDanger = new ListBuffer[Int]()
    for (((dx, dy), i) <- data.zipWithIndex) {
      val distance = Math.abs(x - dx) + Math.abs(y - dy)
      if (distance == lowestDistance) {
        closestDanger += i
      }
      if (distance < lowestDistance) {
        lowestDistance = distance
        closestDanger.clear
        closestDanger += i
      }
    }

    if (closestDanger.size == 1) {
      return closestDanger.head
    } else {
      return -1
    }
  }
}
