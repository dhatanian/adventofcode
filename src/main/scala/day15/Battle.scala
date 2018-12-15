package day15

object Battle {

  def runBattle(grid: Array[Array[Cell]]) = {
    var grid2 = grid
    var elves = 0
    var goblins = 0
    var rounds = 0

    do {
      rounds += 1
      grid2 = runRound(grid2)

      elves = 0
      goblins = 0
      for (y <- grid(0).indices) {
        for (x <- grid.indices) {
          grid2(x)(y) match {
            case CellWithFighter(e@Elf()) => elves += e.health
            case CellWithFighter(g@Goblin()) => goblins += g.health
            case _ =>
          }
        }
      }

      println(raw"Round $rounds, elves $elves, goblins $goblins")
    } while (elves != 0 && goblins != 0 && rounds < 150)

    //Step 1
    println(rounds * (goblins + elves))

    rounds * (goblins + elves)
  }

  def runRound(grid: Array[Array[Cell]]): Array[Array[Cell]] = {
    var moves = MovementLogic.pickActionForFighters(grid)
    val newGrid: Array[Array[Cell]] = Array.fill(grid.length, grid(0).length)(EmptyCell)
    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        newGrid(x)(y) = grid(x)(y)
      }
    }

    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        grid(x)(y) match {
          case CellWithFighter(f) =>
            val move = moves((x, y))
            move match {
              case MoveTo(x2, y2) =>
                newGrid(x2)(y2) match {
                  case EmptyCell =>
                    newGrid(x2)(y2) = grid(x)(y)
                    newGrid(x)(y) = EmptyCell
                  case _ => moves = moves - ((x, y))
                }
              case _ =>
            }
          case _ =>
        }
      }
    }


    //And now the attack
    for (y <- grid(0).indices) {
      for (x <- grid.indices) {
        grid(x)(y) match {
          case CellWithFighter(f) =>
            //See if we have a new position
            val (x1, y1) = moves.getOrElse((x, y), () => NoAction) match {
              case MoveTo(x2, y2) => (x2, y2)
              case _ => (x, y)
            }

            //Pick first enemy in range
            var enemyNeighbours: List[(Int, Int)] = Nil
            for (x2 <- x1 - 1 to x1 + 1 by 2 if x2 >= 0 && x2 < grid.length) {
              val y2 = y1
              newGrid(x2)(y2) match {
                case CellWithFighter(f2) if f2.isEnemyOf(f) => enemyNeighbours = (x2, y2) :: enemyNeighbours
                case _ =>
              }
            }

            for (y2 <- y1 - 1 to y1 + 1 by 2 if y2 >= 0 && y2 < grid(x1).length) {
              val x2 = x1
              newGrid(x2)(y2) match {
                case CellWithFighter(f2) if f2.isEnemyOf(f) => enemyNeighbours = (x2, y2) :: enemyNeighbours
                case _ =>
              }
            }

            if (enemyNeighbours.nonEmpty) {
              val (x2, y2) = enemyNeighbours.minBy(f2 => newGrid(f2._1)(f2._2).asInstanceOf[CellWithFighter].fighter.health)
              val targetEnemyFighter = newGrid(x2)(y2).asInstanceOf[CellWithFighter].fighter
              val newHealth = Math.max(targetEnemyFighter.health - f.attack, 0)
              targetEnemyFighter.health = newHealth
              if (newHealth == 0) {
                newGrid(x2)(y2) = EmptyCell
              } else {
                newGrid(x2)(y2) = CellWithFighter(targetEnemyFighter)
              }
            }
          case _ =>
        }
      }
    }

    newGrid
  }

}
