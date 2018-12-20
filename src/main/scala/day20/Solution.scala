package day20

import de.ummels.dijkstra.SimpleGraph

import scala.collection.mutable

object Solution {

  case class Room(x: Int, y: Int)

  def buildGraphAndFindShortestPaths(r: String) = {
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
    val transitionMap = transitions2.groupBy(_._1).mapValues(_.map(x => (x._2, 1)).toMap)
    val graph = SimpleGraph(transitionMap)
    graph.bfsWithDistance(Room(0, 0))
  }

  //Step 1
  def findDistanceToFurthestRoom(r: String): Int = {
    buildGraphAndFindShortestPaths(r).values.max
  }

  //Step 2
  def findRoomsFurtherThan1000DoorsAway(r: String): Int = {
    buildGraphAndFindShortestPaths(r).count(_._2 >= 1000)
  }

  def main(args: Array[String]) {
    println(findRoomsFurtherThan1000DoorsAway(Data.data))
  }

}
