package day15

trait Cell {
  def toChar(): Char
}

object Wall extends Cell {
  override def toChar(): Char = '#'
}

object EmptyCell extends Cell {
  override def toChar(): Char = '.'
}

case class CellWithFighter(fighter: Fighter) extends Cell {
  override def toChar(): Char = fighter.toChar
}
