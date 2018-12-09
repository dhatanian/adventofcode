package day9

import org.scalatest.prop.TableDrivenPropertyChecks._

class MarbleGameTest extends org.scalatest.FlatSpec {
  val testCases =
    Table(
      ("input", "expectedHighScore"), // First tuple defines column names
      ((9, 25), 32), // Subsequent tuples define the data
      ((13, 7999), 146373),
      ((17, 1104), 2764),
      ((21, 6111), 54718),
      ((30, 5807), 37305)
    )

  forAll(testCases) { (input: (Int, Int), expectedHighScore: Int) => {
    f"${input._1}%s players with last marble ${input._2}%s" should f"return ${expectedHighScore}%s" in {
      assert(new MarbleGame(input._1, input._2).findHighScore() == expectedHighScore)
    }
  }
  }
}
