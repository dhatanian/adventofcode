package day11

object Solution {

  val serialNumber = 9005

  def power(x: Int, y: Int): Long = {
    val rackId = x + 10
    val startPowerLevel = rackId * y
    val increasedPowerLevel = startPowerLevel + serialNumber
    val multipliedPowerLevel = increasedPowerLevel * rackId
    val hundredsDigits = (multipliedPowerLevel / 100) % 10
    hundredsDigits - 5
  }

  def main(args: Array[String]): Unit = {
    val currentPowers = Array.fill(300, 300)(0L)

    for (x <- 1 to 300) {
      for (y <- 1 to 300) {
        currentPowers(x - 1)(y - 1) = power(x, y)
      }
    }

    var highestPowerLevel = Long.MinValue
    var highestPowerLevelCoordinates = (0, 0)
    for (x <- 0 to 297) {
      for (y <- 0 to 297) {
        val totalPower = currentPowers(x)(y) + currentPowers(x + 1)(y) + currentPowers(x + 2)(y) + currentPowers(x)(y + 1) + currentPowers(x + 1)(y + 1) + currentPowers(x + 2)(y + 1) + currentPowers(x)(y + 2) + currentPowers(x + 1)(y + 2) + currentPowers(x + 2)(y + 2)
        if (totalPower > highestPowerLevel) {
          highestPowerLevel = totalPower
          highestPowerLevelCoordinates = (x + 1, y + 1)
        }
      }
    }


    println(highestPowerLevel)
    println(highestPowerLevelCoordinates)
  }
}
