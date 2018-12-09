package day9

class MarbleGame(players: Int, lastMarble: Int) {
  private var marbles = List[Int](0)
  private var currentMarbleIndex = 0
  private var playersScores = Array.fill(players)(0)

  def findHighScore(): Int = {
    for (marble <- 1 to lastMarble) {
      val player = ((marble - 1) % players) + 1
      if (marble % 23 == 0) {
        playersScores(player -1) = marble
      } else {
        currentMarbleIndex = (currentMarbleIndex + 2) % marbles.length
        marbles = (marbles.slice(0, currentMarbleIndex) :+ marble) ++ marbles.slice(currentMarbleIndex, marbles.length)
        println(marbles)
        println(playersScores.toList)
      }
    }

    0
  }
}
