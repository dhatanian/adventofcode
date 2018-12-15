package day15

object GridParser {

  def parseChar(c: Char): Cell = c match {
    case '.' => EmptyCell
    case 'E' => CellWithFighter(Elf())
    case 'G' => CellWithFighter(Goblin())
    case _ => Wall
  }

  def parse(s: String): Array[Array[Cell]] = {
    val tempList = s.stripMargin.split("\n").map(_.trim.toCharArray).toList
    val result: Array[Array[Cell]] = Array.fill(tempList.head.length, tempList.size)(EmptyCell)
    for ((charArray, y) <- tempList.zipWithIndex) {
      for (x <- charArray.indices) {
        result(x)(y) = parseChar(charArray(x))
      }
    }
    result
  }

  def gridAsString(grid: Array[Array[Cell]]): String = {
    var s = ""
    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        s += grid(x)(y).toChar
      }
      if (y != grid(0).length - 1) {
        s += '\n'
      }
    }
    s
  }

}
