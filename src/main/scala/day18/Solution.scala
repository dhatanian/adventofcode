package day18

object Solution {

  val data = Data.data

  def getNeighbours(grid: Array[Array[Char]], x: Int, y: Int): List[Char] =
    (y - 1 to y + 1)
      .intersect(grid(0).indices)
      .flatMap(y2 => (x - 1 to x + 1)
        .intersect(grid(0).indices)
        .filterNot(_ == x && y2 == y)
        .map(grid(_)(y2)))
      .toList

  def tick(grid: Array[Array[Char]]): Array[Array[Char]] = {
    val result = Array.fill(grid.length, grid(0).length)('.')

    for (x <- grid.indices) {
      for (y <- grid(0).indices) {
        val neighbours = getNeighbours(grid, x, y)
        result(x)(y) = grid(x)(y) match {
          case '.' => if (neighbours.count(_ == '|') >= 3) '|' else '.'
          case '|' => if (neighbours.count(_ == '#') >= 3) '#' else '|'
          case '#' => if (neighbours.count(_ == '|') >= 1 && neighbours.count(_ == '#') >= 1) '#' else '.'
        }
      }
    }

    result
  }


  def main(args: Array[String]) {
    var grid = data
    for (_ <- 0 until 10) {
      grid = tick(grid)
      Data.printGrid(grid)
    }

    val trees = grid.map(_.count(_ == '|')).sum
    val lumberyards = grid.map(_.count(_ == '#')).sum
    //Step 1
    println(trees * lumberyards)

//    grid = data
//    for (i <- 0 until 1000000000) {
//      grid = tick(grid)
//      if (i > 100) {
//        val trees = grid.map(_.count(_ == '|')).sum
//        val lumberyards = grid.map(_.count(_ == '#')).sum
//        println(i + "," + trees * lumberyards)
//      }
//    }

    //Step 2
    println(1000000000 % 28)
  }
}
