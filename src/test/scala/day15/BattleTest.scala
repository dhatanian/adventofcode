package day15

import org.scalatest.FunSuite

class BattleTest extends FunSuite {

  test("testRunActualBattleStep1") {

    val input =
      """################################
        |#G..#####G.#####################
        |##.#####...#####################
        |##.#######..####################
        |#...#####.#.#.G...#.##...###...#
        |##.######....#...G..#...####..##
        |##....#....G.........E..####.###
        |#####..#...G........G...##....##
        |######.....G............#.....##
        |######....G.............#....###
        |#####..##.......E..##.#......###
        |########.##...........##.....###
        |####G.G.......#####..E###...####
        |##.......G...#######..#####..###
        |#........#..#########.###...####
        |#.G..GG.###.#########.##...#####
        |#...........#########......#####
        |##..........#########..#.#######
        |###G.G......#########....#######
        |##...#.......#######.G...#######
        |##.......G....#####.E...#.######
        |###......E..G.E......E.....#####
        |##.#................E.#...######
        |#....#...................#######
        |#....#E........E.##.#....#######
        |#......###.#..#..##.#....#..####
        |#...########..#..####....#..####
        |#...########.#########......####
        |#...########.###################
        |############.###################
        |#########....###################
        |################################"""

    Battle.runBattle(GridParser.parse(input))

  }

  test("testRunActualBattleStep2") {

    val input =
      """################################
        |#G..#####G.#####################
        |##.#####...#####################
        |##.#######..####################
        |#...#####.#.#.G...#.##...###...#
        |##.######....#...G..#...####..##
        |##....#....G.........E..####.###
        |#####..#...G........G...##....##
        |######.....G............#.....##
        |######....G.............#....###
        |#####..##.......E..##.#......###
        |########.##...........##.....###
        |####G.G.......#####..E###...####
        |##.......G...#######..#####..###
        |#........#..#########.###...####
        |#.G..GG.###.#########.##...#####
        |#...........#########......#####
        |##..........#########..#.#######
        |###G.G......#########....#######
        |##...#.......#######.G...#######
        |##.......G....#####.E...#.######
        |###......E..G.E......E.....#####
        |##.#................E.#...######
        |#....#...................#######
        |#....#E........E.##.#....#######
        |#......###.#..#..##.#....#..####
        |#...########..#..####....#..####
        |#...########.#########......####
        |#...########.###################
        |############.###################
        |#########....###################
        |################################"""

    //Found by manual dichotomy
    Battle.elvishAttackPower = 34
    println(Battle.runBattleAndCountDeadElves(GridParser.parse(input)))

  }

  test("testRunExampleBattle") {

    val input =
      """#######
        |#.G...#
        |#...EG#
        |#.#.#G#
        |#..G#E#
        |#.....#
        |#######"""

    assert(Battle.runBattle(GridParser.parse(input)) == 27730)
  }

  test("testRunExampleBattle1") {

    val input =
      """#######
        |#G..#E#
        |#E#E.E#
        |#G.##.#
        |#...#E#
        |#...E.#
        |#######"""

    assert(Battle.runBattle(GridParser.parse(input)) == 36334)
  }
  test("testRunExampleBattle2") {

    val input =
      """#######
        |#E..EG#
        |#.#G.E#
        |#E.##E#
        |#G..#.#
        |#..E#.#
        |#######"""

    assert(Battle.runBattle(GridParser.parse(input)) == 39514)
  }
  test("testRunExampleBattle3") {

    val input =
      """#######
        |#E.G#.#
        |#.#G..#
        |#G.#.G#
        |#G..#.#
        |#...E.#
        |#######"""

    assert(Battle.runBattle(GridParser.parse(input)) == 27755)
  }
  test("testRunExampleBattle4") {

    val input =
      """#######
        |#.E...#
        |#.#..G#
        |#.###.#
        |#E#G#G#
        |#...#G#
        |#######"""

    assert(Battle.runBattle(GridParser.parse(input)) == 28944)
  }
  test("testRunExampleBattle5") {

    val input =
      """#########
        |#G......#
        |#.E.#...#
        |#..##..G#
        |#...##..#
        |#...#...#
        |#.G...G.#
        |#.....G.#
        |#########"""

    assert(Battle.runBattle(GridParser.parse(input)) == 18740)
//    var grid = GridParser.parse(input)
//    println(GridParser.gridAsString(grid))
//
//    for(i <- 1 to 22){
//      println(raw"Round $i")
//      Battle.runRound(grid)
//      println(GridParser.gridAsString(grid))
//      println(grid.flatMap(_.toStream).filter({
//        case CellWithFighter(f) => true
//        case _ => false
//      }).map(_.asInstanceOf[CellWithFighter].fighter.health).toList)
//    }
  }

  test("testRound") {

    val input =
      """#######
        |#.G...#
        |#...EG#
        |#.#.#G#
        |#..G#E#
        |#.....#
        |#######"""

    var grid = GridParser.parse(input)
    println(GridParser.gridAsString(grid))

    for(_ <- 0 to 5){
      Battle.runRound(grid)
      println(GridParser.gridAsString(grid))
      println()
    }



  }

}
