package day17

object Solution {

  def flowWater(x: Int, y: Int, grid: Array[Array[Char]]): Unit = {
    if (x < 0 || x >= grid.length || y < 0 || y >= grid(0).length || grid(x)(y) != '.') {

    } else {
      //Have we reached the bottom yet
      if (y == grid(0).length - 1) {
        //mark the grid and be done
        grid(x)(y) = '|'
      } else {
        //let's try to flow down
        if (grid(x)(y + 1) == '.' || grid(x)(y + 1) == '|') {
          grid(x)(y) = '|'
          flowWater(x, y + 1, grid)
        }
        //if it's a wall or water, let's try to expand
        if (grid(x)(y + 1) == '#' || grid(x)(y + 1) == '~') {
          grid(x)(y) = '|'
          if (x > 0 && grid(x - 1)(y) == '.') {
            flowWater(x - 1, y, grid)
          }
          if (x < grid.length - 1 && grid(x + 1)(y) == '.') {
            flowWater(x + 1, y, grid)
          }
        }
        //All other cases can be ignored
      }
    }
  }

  def markWaterAndSweep(grid: Array[Array[Char]]) = {
    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        if (grid(x)(y) == '|') {
          var isContainedWater = false
          if (y < grid(0).length - 1) {
            var firstWallOnLeft = x - 1
            while (firstWallOnLeft >= 0 && grid(firstWallOnLeft)(y) != '#') {
              firstWallOnLeft = firstWallOnLeft - 1
            }
            if (firstWallOnLeft >= 0 && grid(firstWallOnLeft)(y) == '#') {
              var firstWallOnRight = x + 1
              while (firstWallOnRight < grid.length && grid(firstWallOnRight)(y) != '#') {
                firstWallOnRight = firstWallOnRight + 1
              }
              if (firstWallOnRight < grid.length && grid(firstWallOnRight)(y) == '#') {
                isContainedWater = true
                for (x <- firstWallOnLeft to firstWallOnRight if isContainedWater)
                  isContainedWater = grid(x)(y + 1) == '#' || grid(x)(y + 1) == '~'
              }
            }
          }
          if (isContainedWater) {
            grid(x)(y) = '~'
          } else {
            grid(x)(y) = '.'
          }
        }
      }
    }
  }

  def countWater(grid: Array[Array[Char]]): Int = {
    var result = 0
    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        if (grid(x)(y) == '~' || grid(x)(y) == '|')
          result += 1
      }
    }
    result
  }

  def countRetainedWater(grid: Array[Array[Char]]): Int = {
    grid.map(_.count(_ == '~')).sum
  }

  def main(args: Array[String]) {
    val (grid, minX, minY) = Data.data
    var waterCellsCount = 0
    var newWaterCellsCount = 0

    do {
      waterCellsCount = newWaterCellsCount
      markWaterAndSweep(grid)
      pourWater(grid, minX)
      newWaterCellsCount = countWater(grid)
      if (newWaterCellsCount == waterCellsCount) {
        for (_ <- 0 to 9) {
          markWaterAndSweep(grid)
          pourWater(grid, minX)
          newWaterCellsCount = countWater(grid)
        }
      }
    } while (newWaterCellsCount != waterCellsCount)

    Data.printGrid(grid)
    println(waterCellsCount + 1) //Plus one for the top one
    println(countRetainedWater(grid))
  }

  private def pourWater(grid: Array[Array[Char]], minX: Int) = {
    flowWater(500 - minX + 2, 0, grid)
    grid(500 - minX + 2)(0) = '.'
  }
}
