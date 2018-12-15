package day15

import day15.dijkstra.DijkstraMutableWithLists
import de.ummels.dijkstra.SimpleGraph

import scala.collection.mutable

object MovementLogic {

  private def isValidOrigin(cell: Cell) = cell match {
    case EmptyCell => true
    case _ => false
  }

  private def isValidDestination(cell: Cell): Boolean = cell match {
    case EmptyCell => true
    case CellWithFighter(_) => true
    case _ => false
  }

  private def buildDijkstraGraphNodes(grid: Array[Array[Cell]]): (Map[(Int, Int, Cell), Map[(Int, Int, Cell), Int]], List[(Int, Int)]) = {
    val weightMap = mutable.Map.empty[(Int, Int, Cell), mutable.Map[(Int, Int, Cell), Int]].withDefault(_ => mutable.Map.empty[(Int, Int, Cell), Int])
    var fighters: List[(Int, Int)] = Nil

    for (x1 <- grid.indices) {
      for (y1 <- grid(x1).indices) {
        grid(x1)(y1) match {
          case CellWithFighter(_) => fighters = (x1, y1) :: fighters
          case _ =>
        }
        for (x2 <- x1 - 1 to x1 + 1 by 2 if x2 >= 0 && x2 < grid.length) {
          val y2 = y1
          if (isValidOrigin(grid(x1)(y1)) && isValidDestination(grid(x2)(y2))) {
            val w = weightMap((x1, y1, grid(x1)(y1)))
            w.put((x2, y2, grid(x2)(y2)), 1)
            weightMap.put((x1, y1, grid(x1)(y1)), w)
          }
        }
        for (y2 <- y1 - 1 to y1 + 1 by 2 if y2 >= 0 && y2 < grid(x1).length) {
          val x2 = x1
          if (isValidOrigin(grid(x1)(y1)) && isValidDestination(grid(x2)(y2))) {
            val w = weightMap((x1, y1, grid(x1)(y1)))
            w.put((x2, y2, grid(x2)(y2)), 1)
            weightMap.put((x1, y1, grid(x1)(y1)), w)
          }
        }
      }
    }
    (weightMap.mapValues(_.toMap).toMap, fighters)
  }

  private def builGraphForFighter(fighter: (Int, Int), grid: Array[Array[Cell]], graphNodes: Map[(Int, Int, Cell), Map[(Int, Int, Cell), Int]]): SimpleGraph[(Int, Int, Cell)] = {
    val (x1, y1) = fighter
    var nodesWithFighter: Map[(Int, Int, Cell), Int] = Map.empty
    for (x2 <- x1 - 1 to x1 + 1 by 2 if x2 >= 0 && x2 < grid.length) {
      val y2 = y1
      if (isValidDestination(grid(x2)(y2))) {
        nodesWithFighter += ((x2, y2, grid(x2)(y2)) -> 1)
      }
    }
    for (y2 <- y1 - 1 to y1 + 1 by 2 if y2 >= 0 && y2 < grid(x1).length) {
      val x2 = x1
      if (isValidDestination(grid(x2)(y2))) {
        nodesWithFighter += ((x2, y2, grid(x2)(y2)) -> 1)
      }
    }
    SimpleGraph(graphNodes + ((x1, y1, grid(x1)(y1)) -> nodesWithFighter))
  }

  private def pathsToChosenPredecessor(currentCell: (Int, Int, Cell), predecessors: Map[(Int, Int, Cell), List[(Int, Int, Cell)]], target: (Int, Int)): Set[(Int, Int)] = {
    predecessors(currentCell) match {
      case List((target._1, target._2, _)) => Set((currentCell._1, currentCell._2))
      case Nil => Set.empty
      case l => l.flatMap(p => pathsToChosenPredecessor(p, predecessors, target)).toSet
    }
  }

  private def returnNoActionIfEnemyNeighbourPresent(fighterCoordinates: (Int, Int), grid: Array[Array[Cell]]): Option[Action] = {
    var enemyNeighbours: List[(Int, Int)] = Nil
    val (x1, y1) = fighterCoordinates
    val f1 = grid(x1)(y1).asInstanceOf[CellWithFighter].fighter
    for (x2 <- x1 - 1 to x1 + 1 by 2 if x2 >= 0 && x2 < grid.length) {
      val y2 = y1
      grid(x2)(y2) match {
        case CellWithFighter(f2) if f2.isEnemyOf(f1) => enemyNeighbours = (x2, y2) :: enemyNeighbours
        case _ =>
      }
    }

    for (y2 <- y1 - 1 to y1 + 1 by 2 if y2 >= 0 && y2 < grid(x1).length) {
      val x2 = x1
      grid(x2)(y2) match {
        case CellWithFighter(f2) if f2.isEnemyOf(f1) => enemyNeighbours = (x2, y2) :: enemyNeighbours
        case _ =>
      }
    }

    if (enemyNeighbours.isEmpty) None else Some(NoAction)
  }

  def pickActionForFighters(grid: Array[Array[Cell]]): Map[(Int, Int), Action] = {
    val (graphNodes, fighters) = buildDijkstraGraphNodes(grid)
    var result: Map[(Int, Int), Action] = Map.empty

    for (fighter <- fighters) {
      result += fighter -> returnNoActionIfEnemyNeighbourPresent(fighter, grid).getOrElse(findNextMove(grid, graphNodes, fighter))
    }

    result
  }

  def pickActionForFighter(grid: Array[Array[Cell]], fighter: (Int, Int)): Action = {
    val (graphNodes, _) = buildDijkstraGraphNodes(grid)
    returnNoActionIfEnemyNeighbourPresent(fighter, grid).getOrElse(findNextMove(grid, graphNodes, fighter))
  }

  private def findNextMove(grid: Array[Array[Cell]], graphNodes: Map[(Int, Int, Cell), Map[(Int, Int, Cell), Int]], fighter: (Int, Int)): Action = {
    val graph = builGraphForFighter(fighter, grid, graphNodes)
    val (x, y) = fighter
    val (distances, predecessors) = DijkstraMutableWithLists.dijkstra(graph)((x, y, grid(x)(y)))
    val distancesToEnemies = distances.filterKeys({
      case (_, _, CellWithFighter(f)) => f.isEnemyOf(grid(x)(y).asInstanceOf[CellWithFighter].fighter)
      case _ => false
    })
    if (distancesToEnemies.isEmpty) {
      NoAction
    } else {
      val distanceToClosestEnemy = distancesToEnemies.minBy(_._2)._2
      val closestEnemies = distancesToEnemies.filter(_._2 == distanceToClosestEnemy).keys
      val predecessorsOfClosestEnemies = closestEnemies.flatMap(predecessors)
      val firstPredecessorInReadingOrder = predecessorsOfClosestEnemies.minBy(p => (p._2, p._1))
      val possibleMoves = pathsToChosenPredecessor(firstPredecessorInReadingOrder, predecessors, fighter)
      val (x1, y1) = possibleMoves.minBy(p => (p._2, p._1))
      MoveTo(x1, y1)
    }
  }
}
