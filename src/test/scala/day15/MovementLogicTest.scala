package day15

import org.scalatest.FunSuite

class MovementLogicTest extends FunSuite {

  test("with walls") {
    val input =
      """#######
        |#E..G.#
        |#...#.#
        |#.G.#G#
        |#######"""

    assert(findNextMove(input) == Map((5, 3) -> NoAction, (4, 1) -> MoveTo(3, 1), (2, 3) -> MoveTo(2, 2), (1, 1) -> MoveTo(2, 1)))
  }

  test("simple test") {
    val input =
      """#######
        |#.E...#
        |#.....#
        |#...G.#
        |#######"""

    assert(findNextMove(input) == Map((4, 3) -> MoveTo(4, 2), (2, 1) -> MoveTo(3, 1)))
  }


  private def findNextMove(input: String) = {
    MovementLogic.pickActionForFighters(GridParser.parse(input))
  }
}
