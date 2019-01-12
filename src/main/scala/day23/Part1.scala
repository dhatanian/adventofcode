package day23

object Part1 {

  val bots = Data.data

  def distance(n1: Data.Nanobot, n2: Data.Nanobot) = math.abs(n1.x - n2.x) + math.abs(n1.y - n2.y) + math.abs(n1.z - n2.z)

  def distance(position: (Int, Int, Int), n: Data.Nanobot) = math.abs(n.x - position._1) + math.abs(n.y - position._2) + math.abs(n.z - position._3)

  def intersectionsAtPosition(position: (Int, Int, Int)): Int = bots.count(b => distance(position, b) <= b.r)

  def main(args: Array[String]): Unit = {
    val nanobotWithLongestRange = bots.maxBy(_.r)

    val nanobotsInRange = bots.filter(distance(_, nanobotWithLongestRange) <= nanobotWithLongestRange.r)

    println(nanobotsInRange.length)
  }


}
