package day20

import de.ummels.dijkstra.{DijkstraMutable, SimpleGraph}

import scala.collection.mutable

object Solution {

  case class Room(x: Int, y: Int)

  def findDistanceToFurthestRoom(r: String): Int = {
    val transitions: mutable.Set[(Room, Room)] = mutable.Set.empty
    
    var previousRooms = Set(Room(0, 0))
    var roomsBeforeBracket: List[Set[Room]] = List(Set.empty)
    var roomsInOrClause: List[Set[Room]] = Nil
    var i = 0

    def updateTransitions(dx: Int, dy: Int) = {
      val newRooms = previousRooms.map({ case Room(x, y) => Room(x + dx, y + dy) })
      val newTransitions = previousRooms.zip(newRooms)
      newTransitions.foreach(transitions.add)
      previousRooms = newRooms
    }

    for (c <- r) {
      i += 1
      println(raw"$i / ${r.length}")
      c match {
        case 'W' =>
          updateTransitions(-1, 0)
        case 'N' =>
          updateTransitions(0, -1)
        case 'S' =>
          updateTransitions(0, 1)
        case 'E' =>
          updateTransitions(1, 0)
        case '|' =>
          roomsInOrClause = previousRooms ++ roomsInOrClause.head :: roomsInOrClause.tail
          previousRooms = roomsBeforeBracket.head
        case '(' =>
          roomsBeforeBracket = previousRooms :: roomsBeforeBracket
          roomsInOrClause = Set.empty[Room] :: roomsInOrClause
        case ')' =>
          roomsInOrClause = previousRooms ++ roomsInOrClause.head :: roomsInOrClause.tail
          roomsBeforeBracket = roomsBeforeBracket.tail
          previousRooms = roomsInOrClause.head
          roomsInOrClause = roomsInOrClause.tail
      }
    }
    val transitions2 = transitions ++ transitions.map(t => (t._2, t._1))
    val uniqueDoors = transitions.filterNot(t => transitions contains ((t._2, t._1))).toList.sortBy(t => (t._1.y, t._1.x, t._2.y, t._2.x))
//    println(uniqueDoors.mkString("\n"))
    val transitionMap = transitions2.groupBy(_._1).mapValues(_.map(x => (x._2, 1)).toMap)
    val graph = SimpleGraph(transitionMap)
    DijkstraMutable.dijkstra(graph)(Room(0, 0))._1.values.max
  }

  def main(args: Array[String]) {
    println(findDistanceToFurthestRoom(Data.data))
  }

}
