package day22

import day22.Solution.Equipment.{ClimbingGear, Nothing, Torch}
import de.ummels.dijkstra.{DijkstraMutable, Graph, SimpleGraph}

import scala.collection.mutable

object Solution {
  val (depth, (targetX, targetY)) = Data.data

  def main(args: Array[String]) {
    //Part 1
    val gridStep1: Array[Array[Int]] = buildGrid()
    println(gridStep1.map(_.sum).sum)

    //Part 2
    val grid = buildGrid(10 * targetX, 2 * targetY)
    val graph: Graph[Section] = buildGraph(grid)
    val dijkstraResult = DijkstraMutable.dijkstra(graph)(Section(0, 0, Torch))
    println(dijkstraResult._1(Section(targetX, targetY, Torch)))
  }

  object Equipment {

    sealed trait EnumVal

    case object Nothing extends EnumVal

    case object Torch extends EnumVal

    case object ClimbingGear extends EnumVal

  }

  case class Section(x: Int, y: Int, mode: Equipment.EnumVal)

  def validEquipments(grid: Array[Array[Int]], x: Int, y: Int): List[Equipment.EnumVal] = if (x == 0 && y == 0) {
    List(Torch)
  } else if (x == targetX && y == targetY) {
    List(Torch)
  } else {
    grid(x)(y) match {
      case 0 => List(ClimbingGear, Torch)
      case 1 => List(ClimbingGear, Nothing)
      case 2 => List(Torch, Nothing)
    }
  }

  def findNeighbours(grid: Array[Array[Int]], x: Int, y: Int): List[(Int, Int)] = {
    var result: List[(Int, Int)] = Nil

    for (x2 <- x - 1 to x + 1 by 2 if x2 >= 0 && x2 < grid.length) {
      result = (x2, y) :: result
    }

    for (y2 <- y - 1 to y + 1 by 2 if y2 >= 0 && y2 < grid(0).length) {
      result = (x, y2) :: result
    }

    result
  }

  def timeSwitchingEquipment(s1: Section, s2: Section, grid: Array[Array[Int]]): Int = if (s1.mode == s2.mode) 0 else 7

  def buildGraph(grid: Array[Array[Int]]): de.ummels.dijkstra.Graph[Section] = {
    val succ: mutable.Map[Section, Map[Section, Int]] = mutable.Map.empty

    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        println((x, y))
        val possibleEquipments = validEquipments(grid, x, y)
        val neighbours = findNeighbours(grid, x, y)
        val neighboursSections = neighbours
          .zip(neighbours.map(n => validEquipments(grid, n._1, n._2) intersect possibleEquipments))
          .map(t => t._2.map(Section(t._1._1, t._1._2, _)))
        for (e <- possibleEquipments) {
          val section = Section(x, y, e)
          val neighboursSectionsWithDistances = neighboursSections.map(sections => {
            val sectionsWithDistance = sections.map(s => (s, 1 + timeSwitchingEquipment(section, s, grid)))
            if(sectionsWithDistance.isEmpty){
              sectionsWithDistance
            }else {
              val minDistance = sectionsWithDistance.map(_._2).min
              sectionsWithDistance.filter(_._2 == minDistance).toMap
            }
          })
          neighboursSectionsWithDistances.flatten.foreach({ case (s, d) =>
            val currentSuccessors = succ.getOrElse(section, Map.empty[Section, Int])
            succ.put(section, currentSuccessors + (s -> d))
          })
        }
      }
    }

    SimpleGraph[Section](succ.toMap)
  }

  private def printGrid(grid: Array[Array[Int]]) = {
    for (y <- 0 to grid(0).length) {
      for (x <- 0 to grid.length) {
        if (x == 0 && y == 0) {
          print('M')
        } else if (x == targetX && y == targetY) {
          print('T')
        } else {
          print(grid(x)(y) match {
            case 0 => '.'
            case 1 => '='
            case 2 => '|'
          })
        }
      }
      println()
    }
    println()
  }

  private def buildGrid(maxX: Int = targetX, maxY: Int = targetY) = {
    val erosionLevels = Array.fill(maxX + 1, maxY + 1)(0)
    val riskLevels = Array.fill(maxX + 1, maxY + 1)(0)


    for (x <- 0 to maxX) {
      for (y <- 0 to maxY) {
        if (x == 0 && y == 0) {
          erosionLevels(x)(y) = depth % 20183
        } else if (x == targetX && y == targetY) {
          erosionLevels(x)(y) = depth % 20183
        } else {
          val geologicIndex = if (x == 0) y * 48271 else if (y == 0) x * 16807 else erosionLevels(x - 1)(y) * erosionLevels(x)(y - 1)

          erosionLevels(x)(y) = (geologicIndex + depth) % 20183
        }
        riskLevels(x)(y) = erosionLevels(x)(y) % 3
      }
    }

    riskLevels
  }
}
