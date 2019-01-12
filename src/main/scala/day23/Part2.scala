package day23

import day23.Data.Nanobot

import scala.collection.immutable

object Part2 {

  val bots = Data.data

  def distanceToZero(t: (Int, Int, Int)): Int = math.abs(t._1) + math.abs(t._2) + math.abs(t._3)

  def isInRange(x: Int, y: Int, z: Int, h: Int, b: Nanobot): Boolean = {
    val distanceX = if (x > b.x) x - b.x else if (x + h - 1 < b.x) b.x - x - h + 1 else 0
    val distanceY = if (y > b.y) y - b.y else if (y + h - 1 < b.y) b.y - y - h + 1 else 0
    val distanceZ = if (z > b.z) z - b.z else if (z + h - 1 < b.z) b.z - z - h + 1 else 0
    distanceX + distanceY + distanceZ <= b.r
  }

  def numberOfBotsInRange(x: Int, y: Int, z: Int, h: Int) = {
    bots.count(isInRange(x, y, z, h, _))
  }

  def main(args: Array[String]): Unit = {
    val minX = bots.map(_.x).min
    val maxX = bots.map(_.x).max
    val minY = bots.map(_.y).min
    val maxY = bots.map(_.y).max
    val minZ = bots.map(_.z).min
    val maxZ = bots.map(_.z).max

    val min = math.min(minX, math.min(minY, minZ))
    val max = math.max(maxX, math.max(maxY, maxZ))

    val step = 2

    var n = 0
    while (math.pow(step, n) < max - min + 1) {
      n = n + 1
    }

    var currentOctrees: immutable.List[(Int, Int, Int, Int)] = List((min, min, min, math.pow(step, n).toInt))
    var nextOctrees: immutable.List[(Int, Int, Int, Int)] = Nil
    while (n > 0) {
      var bestScore = 0
      while (currentOctrees.nonEmpty) {
        val (x, y, z, h) = currentOctrees.head
        currentOctrees = currentOctrees.tail

        val h2 = h / step
        for (x2 <- 0 until step) {
          for (y2 <- 0 until step) {
            for (z2 <- 0 until step) {
              val count = numberOfBotsInRange(x + x2 * h2, y + y2 * h2, z + z2 * h2, h2)
              if (count == bestScore) {
                nextOctrees = (x + x2 * h2, y + y2 * h2, z + z2 * h2, h2) :: nextOctrees
              } else if (count > bestScore) {
                nextOctrees = (x + x2 * h2, y + y2 * h2, z + z2 * h2, h2) :: Nil
                bestScore = count
              }
            }
          }
        }
      }
      println(s"Octrees found with size $step^$n : ${nextOctrees.size}, best score $bestScore")
      currentOctrees = nextOctrees
      nextOctrees = Nil
      n = n - 1
    }

    println(currentOctrees)
    println(currentOctrees.size)
    val minDistanceToOrigin = currentOctrees.map(t => distanceToZero((t._1, t._2, t._3))).min
    println(minDistanceToOrigin)
  }


}
