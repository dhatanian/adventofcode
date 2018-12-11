package day11

object Solution {

  val serialNumber = 9005

  val currentPowers = Array.fill(300, 300)(0L)

  def power(x: Int, y: Int): Long = {
    val rackId = x + 10
    val startPowerLevel = rackId * y
    val increasedPowerLevel = startPowerLevel + serialNumber
    val multipliedPowerLevel = increasedPowerLevel * rackId
    val hundredsDigits = (multipliedPowerLevel / 100) % 10
    hundredsDigits - 5
  }

  def highestPowerLevelForGridSize(gridSize: Int) = {
    println(gridSize)
    var highestPowerLevel = Long.MinValue
    var highestPowerLevelCoordinates = (0, 0)
    for (x <- 0 to 300 - gridSize) {
      for (y <- 0 to 300 - gridSize) {
        val totalPower = (0 until gridSize).flatMap(dx => (0 until gridSize).map(dy => currentPowers(x + dx)(y + dy))).sum
        if (totalPower > highestPowerLevel) {
          highestPowerLevel = totalPower
          highestPowerLevelCoordinates = (x + 1, y + 1)
        }
      }
    }
    (highestPowerLevel, highestPowerLevelCoordinates)
  }

  def main(args: Array[String]): Unit = {
    for (x <- 1 to 300) {
      for (y <- 1 to 300) {
        currentPowers(x - 1)(y - 1) = power(x, y)
      }
    }

    val result = (1 to 300).map(highestPowerLevelForGridSize).maxBy(_._1)

    println(result)
  }
}
