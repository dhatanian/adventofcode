package day25

import org.scalatest.prop.TableDrivenPropertyChecks._

class ConstellationsCounterTest extends org.scalatest.FlatSpec {
  val testCases =
    Table(
      ("input", "expectedCount"), // First tuple defines column names
      ("example1.txt", 2), // Subsequent tuples define the data
      ("example2.txt", 4),
      ("example3.txt", 3),
      ("example4.txt", 8)
    )

  forAll(testCases) { (input: String, expectedCount: Int) => {
    input should s"return $expectedCount" in {
      assert(ConstellationsCounter.constellationsCount(DataParser.parse(input)) == expectedCount)
    }
  }
  }

}
