package day15

abstract class Fighter(var health: Int = 200, var attack: Int = 3) {
  def withNewHealth(newHealth: Int): Fighter = {
    this.health = newHealth
    this
  }

  def isEnemyOf(fighter: Fighter): Boolean

  def pickNextAction(grid: Array[Array[Cell]], x: Int, y: Int): Action = {
    MoveTo(1, 2)
  }

  def toChar: Char
}

case class Goblin() extends Fighter {
  override def toChar: Char = 'G'

  override def isEnemyOf(fighter: Fighter): Boolean = fighter match {
    case Elf() => true
    case _ => false
  }
}

case class Elf() extends Fighter {
  override def toChar: Char = 'E'

  override def isEnemyOf(fighter: Fighter): Boolean = fighter match {
    case Goblin() => true
    case _ => false
  }
}
