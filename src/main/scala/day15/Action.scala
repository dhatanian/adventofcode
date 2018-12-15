package day15

trait Action

object NoAction extends Action

case class Attack(x: Int, y: Int) extends Action

case class MoveTo(x: Int, y: Int) extends Action
