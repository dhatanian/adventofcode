package day17

import scala.io.Source

object Data {
  val coordinatesRegex = raw"([x|y])=(\d+), ([x|y])=(\d+)\.\.(\d+)".r

  def parse(strings: List[String]) = {
    val data = strings
      .filterNot(_.trim.isEmpty)
      .map(coordinatesRegex.unapplySeq)
      .map(_.get)
      .map(l =>
        if (l.head == "x")
          ((l(1).toInt, l(1).toInt), (l(3).toInt, l(4).toInt))
        else
          ((l(3).toInt, l(4).toInt), (l(1).toInt, l(1).toInt))
      )

    val minX = data.map(_._1._1).min
    val minY = data.map(_._2._1).min
    val maxX = data.map(_._1._2).max
    val maxY = data.map(_._2._2).max

    val grid = Array.fill(maxX - minX + 3, maxY - minY + 1)('.')

    data.foreach({ case ((x1, x2), (y1, y2)) =>
      for (x <- x1 - minX to x2 - minX)
        for (y <- y1 - minY to y2 - minY)
          grid(x + 1)(y) = '#'
    })

    (grid, minX, minY)
  }

  def printGrid(grid: Array[Array[Char]]) = {
    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        print(grid(x)(y))
      }
      println()
    }
    println()
  }

  val example = parse(
    """x=495, y=2..7
      |y=7, x=495..501
      |x=501, y=3..7
      |x=498, y=2..4
      |x=506, y=1..2
      |x=498, y=10..13
      |x=504, y=10..13
      |y=13, x=498..504""".stripMargin.split("\n").toList)

  val data = parse(Source.fromInputStream(getClass.getResourceAsStream("data.txt")).getLines().toList)

  def main(args: Array[String]) {
    printGrid(example._1)
  }
}
