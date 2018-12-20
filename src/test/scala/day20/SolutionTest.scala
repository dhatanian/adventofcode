package day20

import org.scalatest.prop.TableDrivenPropertyChecks._

class SolutionTest extends org.scalatest.FlatSpec {
  val testCases =
    Table(
      ("regex", "expectedDistanceToFurthestRoom"), // First tuple defines column names
      ("WNE", 3),
      ("ENWWW(NEEE|SSE(EE|N))", 10),
      ("ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN", 18),
      ("ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))", 23),
      ("WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))", 31)
    )

  forAll(testCases) {
    (regex: String, expectedDistanceToFurthestRoom: Int) => {
      regex should raw"return distance to furthest room $expectedDistanceToFurthestRoom" in {
        assert(Solution.findDistanceToFurthestRoom(regex) == expectedDistanceToFurthestRoom)
      }
    }
  }

}
