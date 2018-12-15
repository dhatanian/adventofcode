package day15

import scala.collection.immutable.Queue

object Battle {
  var elvishAttackPower: Int = 3

  //Step 2
  def runBattleAndCountDeadElves(grid: Array[Array[Cell]]) = {
    var elvesCount = 0

    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        grid(x)(y) match {
          case CellWithFighter(e@Elf()) => elvesCount += 1
          case _ =>
        }
      }
    }

    var outcome = runBattle(grid)

    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        grid(x)(y) match {
          case CellWithFighter(e@Elf()) => elvesCount -= 1
          case _ =>
        }
      }
    }
    (outcome, elvesCount)
  }

  def runBattle(grid: Array[Array[Cell]]) = {
    var rounds = 0

    try {
      do {
        runRound(grid)
        rounds += 1

        var elves = 0
        var goblins = 0
        for (y <- grid(0).indices) {
          for (x <- grid.indices) {
            grid(x)(y) match {
              case CellWithFighter(e@Elf()) => elves += e.health
              case CellWithFighter(g@Goblin()) => goblins += g.health
              case _ =>
            }
          }
        }

//        println(raw"Round $rounds, elves $elves, goblins $goblins")
//        println(GridParser.gridAsString(grid))
//        println
//        println

      } while (rounds < 150)
    } catch {
      case e: GameEndedException =>
//        println("Game ended during round")
    }


    var elves = 0
    var goblins = 0
    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        grid(x)(y) match {
          case CellWithFighter(e@Elf()) => elves += e.health
          case CellWithFighter(g@Goblin()) => goblins += g.health
          case _ =>
        }
      }
    }

    println(raw"Round $rounds, elves $elves, goblins $goblins")

    //Step 1
//    println(rounds * (goblins + elves))

    rounds * (goblins + elves)
  }

  def checkGameEnded(grid: Array[Array[Cell]]): Unit = {
    var elves = 0
    var goblins = 0
    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        grid(x)(y) match {
          case CellWithFighter(e@Elf()) => elves += e.health
          case CellWithFighter(g@Goblin()) => goblins += g.health
          case _ =>
        }
      }
    }

    if (elves == 0 || goblins == 0) {
      throw new GameEndedException()
    }

  }

  def runRound(grid: Array[Array[Cell]]): Unit = {
    var fightersInOrder: Queue[(Int, Int)] = Queue.empty

    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        grid(x)(y) match {
          case CellWithFighter(_) =>
            fightersInOrder = fightersInOrder.enqueue((x, y))
          case _ =>
        }
      }
    }

    for ((x, y) <- fightersInOrder) {
      grid(x)(y) match {
        //Checking we're not dead
        case CellWithFighter(f) => {
          checkGameEnded(grid)

          //Move
          val moveAction = MovementLogic.pickActionForFighter(grid, (x, y))
          val (x1, y1) = moveAction match {
            case MoveTo(x2, y2) =>
              grid(x2)(y2) = grid(x)(y)
              grid(x)(y) = EmptyCell
              (x2, y2)
            case _ => (x, y)
          }

          //Attack
          //Pick first enemy in range
          var enemyNeighbours: List[(Int, Int)] = Nil
          for (x2 <- x1 - 1 to x1 + 1 by 2 if x2 >= 0 && x2 < grid.length) {
            val y2 = y1
            grid(x2)(y2) match {
              case CellWithFighter(f2) if f2.isEnemyOf(f) => enemyNeighbours = (x2, y2) :: enemyNeighbours
              case _ =>
            }
          }

          for (y2 <- y1 - 1 to y1 + 1 by 2 if y2 >= 0 && y2 < grid(x).length) {
            val x2 = x1
            grid(x2)(y2) match {
              case CellWithFighter(f2) if f2.isEnemyOf(f) => enemyNeighbours = (x2, y2) :: enemyNeighbours
              case _ =>
            }
          }

          if (enemyNeighbours.nonEmpty) {
            val (x2, y2) = enemyNeighbours.minBy(f2 => (grid(f2._1)(f2._2).asInstanceOf[CellWithFighter].fighter.health, f2._2, f2._1))
            val targetEnemyFighter = grid(x2)(y2).asInstanceOf[CellWithFighter].fighter
            val newHealth = Math.max(targetEnemyFighter.health - f.attack, 0)
            targetEnemyFighter.health = newHealth
            if (newHealth == 0) {
              grid(x2)(y2) = EmptyCell
            } else {
              grid(x2)(y2) = CellWithFighter(targetEnemyFighter)
            }
          }
        }
        case _ =>
      }
    }
  }

}

class GameEndedException() extends Exception
