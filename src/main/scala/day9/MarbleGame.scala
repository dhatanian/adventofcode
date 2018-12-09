package day9

class Marble {
  var value: Long = 0
  var next: Marble = this
  var previous: Marble = this

  def this(value: Long) {
    this()
    this.value = value
  }

  def this(value: Long, next: Marble, previous: Marble) {
    this(value)
    this.next = next
    this.previous = previous
  }
}

class MarbleGame(players: Int, lastMarble: Long) {
  private var currentMarble = new Marble(0L)
  private val playersScores = Array.fill(players)(0L)

  def findHighScore(): Long = {
    for (marbleValue <- 1L to lastMarble) {
      val player = (((marbleValue - 1) % players) + 1).toInt
      if (marbleValue % 23L == 0) {
        for (_ <- 1 to 7) {
          currentMarble = currentMarble.previous
        }
        playersScores(player - 1) = playersScores(player - 1) + marbleValue + currentMarble.value
        currentMarble.previous.next = currentMarble.next
        currentMarble.next.previous = currentMarble.previous
        currentMarble = currentMarble.next
      } else {
        currentMarble = currentMarble.next
        val newMarble = new Marble(marbleValue, currentMarble.next, currentMarble)
        newMarble.next.previous = newMarble
        newMarble.previous.next = newMarble
        currentMarble = newMarble
      }
    }

    playersScores.max
  }
}
