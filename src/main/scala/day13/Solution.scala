package day13

import scala.collection.immutable.Queue

object Solution {

  case class Cell(transition: Transition.Value, cart: Option[Cart])

  case class Cart(dx: Int, dy: Int, nextIntersectionAction: IntersectionAction.Value = IntersectionAction.Left)

  object IntersectionAction extends Enumeration {
    val Left, Straight, Right = Value
  }

  object Transition extends Enumeration {
    val -, |, /, \, X, + = Value
  }

  def parse(state: String): Array[Array[Cell]] = {
    val splitState = state.split("\n")
    val maxX = splitState.map(_.length).max
    val track: Array[Array[Cell]] = Array.fill(maxX, splitState.length)(Cell(Transition.X, None))
    var carts: List[Cart] = Nil

    for (y <- 0 until splitState.length) {
      for (x <- 0 until splitState(y).length) {
        val character = splitState(y)(x)
        track(x)(y) = character match {
          case '-' => Cell(Transition.-, None)
          case '>' => Cell(Transition.-, Some(Cart(1, 0)))
          case '<' => Cell(Transition.-, Some(Cart(-1, 0)))
          case '|' => Cell(Transition.|, None)
          case 'v' => Cell(Transition.|, Some(Cart(0, 1)))
          case '^' => Cell(Transition.|, Some(Cart(0, -1)))
          case '/' => Cell(Transition./, None)
          case '\\' => Cell(Transition.\, None)
          case '+' => Cell(Transition.+, None)
          case _ => Cell(Transition.X, None)
        }
      }
    }

    track
  }

  //  val track: Array[Array[Cell]] = parse(Example.initialState)
  val track: Array[Array[Cell]] = parse(Data.initialState)

  def rotateCart(cart: Cart, transition: Transition.Value): Cart = transition match {
    case Transition.- | Transition.| => cart
    case Transition.\ => if (cart.dx != 0) Cart(0, cart.dx, cart.nextIntersectionAction) else Cart(cart.dy, 0, cart.nextIntersectionAction)
    case Transition./ => if (cart.dx != 0) Cart(0, -cart.dx, cart.nextIntersectionAction) else Cart(-cart.dy, 0, cart.nextIntersectionAction)
    case Transition.+ => cart.nextIntersectionAction match {
      case IntersectionAction.Straight => cart.copy(nextIntersectionAction = IntersectionAction.Right)
      case IntersectionAction.Left => if (cart.dx != 0) Cart(0, -cart.dx, IntersectionAction.Straight) else Cart(cart.dy, 0, IntersectionAction.Straight)
      case IntersectionAction.Right => if (cart.dx != 0) Cart(0, cart.dx, IntersectionAction.Left) else Cart(-cart.dy, 0, IntersectionAction.Left)
    }
  }

  def moveCart(x: Int, y: Int) = {
    val currentCell = track(x)(y)
    if (currentCell.cart.isDefined) {
      val dx = currentCell.cart.get.dx
      val dy = currentCell.cart.get.dy
      val newCell = track(x + dx)(y + dy)
      newCell match {
        case Cell(_, Some(cart)) => {
          //First step : this is where we catch the first crash
          println(raw"Crash at ${x + dx},${y + dy}")
          track(x)(y) = currentCell.copy(cart = None)
          track(x + dx)(y + dy) = newCell.copy(cart = None)
        }
        case Cell(_, None) =>
          track(x)(y) = currentCell.copy(cart = None)
          track(x + dx)(y + dy) = newCell.copy(cart = Some(rotateCart(currentCell.cart.get, newCell.transition)))
      }
    }
  }

  def tick(): Unit = {
    var carPositions: Queue[(Int, Int)] = Queue()
    for (y <- track(0).indices) {
      for (x <- track.indices) {
        track(x)(y) match {
          case Cell(_, Some(_)) => carPositions = carPositions :+ (x, y)
          case _ =>
        }
      }
    }

    carPositions match {
      case q :+ car if q.isEmpty => println(raw"Last car at ${car}")
      case _ => carPositions.foreach(car => moveCart(car._1, car._2))
    }
  }

  def printCart(dx: Int, dy: Int) = if (dx > 0) ">" else if (dx < 0) "<" else if (dy > 0) "v" else "^"

  def printTrack(): Unit = {
    for (y <- track(0).indices) {
      for (x <- track.indices) {
        val character = track(x)(y) match {
          case Cell(_, Some(Cart(dx, dy, _))) => printCart(dx, dy)
          case Cell(Transition.-, None) => "-"
          case Cell(Transition.|, None) => "|"
          case Cell(Transition./, None) => "/"
          case Cell(Transition.\, None) => "\\"
          case Cell(Transition.X, None) => " "
          case Cell(Transition.+, None) => "+"
          case _ => throw new Exception("Unkown cell")
        }
        print(character)
      }
      println()
    }
    println()
    println()
  }

  def main(args: Array[String]): Unit = {
    while (true) {
      //printTrack()
      tick()
    }
  }
}
